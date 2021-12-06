/*
Copyright (C) 2021 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

This file is based on the emacs-sqlite3 package written by Syohei
YOSHIDA <syohex@gmail.com>, which can be found at:

   https://github.com/syohex/emacs-sqlite3
*/

#include <config.h>
#include "lisp.h"

#ifdef HAVE_SQLITE3

#include <sqlite3.h>

static void
sqlite_free (void *arg)
{
  struct Lisp_Sqlite *ptr = (struct Lisp_Sqlite *)arg;
  if (ptr->is_statement)
    sqlite3_finalize (ptr->stmt);
  else if (ptr->db)
    sqlite3_close (ptr->db);
  xfree (ptr);
}

static Lisp_Object
make_sqlite (bool is_statement, void *db, void *stmt, Lisp_Object columns)
{
  struct Lisp_Sqlite *ptr
    = ALLOCATE_ZEROED_PSEUDOVECTOR (struct Lisp_Sqlite, columns, PVEC_SQLITE);
  ptr->is_statement = is_statement;
  ptr->finalizer = sqlite_free;
  ptr->db = db;
  ptr->stmt = stmt;
  ptr->columns = columns;
  ptr->eof = false;
  return make_lisp_ptr (ptr, Lisp_Vectorlike);
}

DEFUN ("sqlite-open", Fsqlite_open, Ssqlite_open, 0, 1, 0,
       doc: /* Open FILE as an sqlite database.
If FILE is nil, an in-memory database will be opened instead.  */)
  (Lisp_Object file)
{
  if (!NILP (file))
    {
      CHECK_STRING (file);
      file = Fexpand_file_name (file, Qnil);
    }

  sqlite3 *sdb;
  int ret = sqlite3_open_v2 (NILP (file) ? ":memory:" : SSDATA (file),
			     &sdb,
			     SQLITE_OPEN_FULLMUTEX
			     | SQLITE_OPEN_READWRITE
			     | SQLITE_OPEN_CREATE
#ifdef SQLITE_OPEN_URI
			     | SQLITE_OPEN_URI
#endif
			     | 0, NULL);
  if (ret != SQLITE_OK)
    return Qnil;

  return make_sqlite (false, sdb, NULL, Qnil);
}

/* Bind values in a statement like
   "insert into foo values (?, ?, ?)".  */
static const char *
bind_values (sqlite3 *db, sqlite3_stmt *stmt, Lisp_Object values)
{
  sqlite3_reset (stmt);
  int len = ASIZE (values);

  for (int i = 0; i < len; ++i)
    {
      int ret = SQLITE_MISMATCH;
      Lisp_Object value = AREF (values, i);
      Lisp_Object type = Ftype_of (value);

      if (EQ (type, Qstring))
	ret = sqlite3_bind_text (stmt, i + 1,
				 SSDATA (value), SBYTES (value),
				 NULL);
      else if (EQ (type, Qinteger))
	ret = sqlite3_bind_int64 (stmt, i + 1, XFIXNUM (value));
      else if (EQ (type, Qfloat))
	ret = sqlite3_bind_double (stmt, i + 1, XFLOAT_DATA (value));
      else if (NILP (value))
	ret = sqlite3_bind_null (stmt, i + 1);
      else if (EQ (value, Qt))
	ret = sqlite3_bind_int (stmt, i + 1, 1);
      else if (EQ (value, Qfalse))
	ret = sqlite3_bind_int (stmt, i + 1, 0);
      else
	return "invalid argument";

      if (ret != SQLITE_OK)
	return sqlite3_errmsg (db);
    }

  return NULL;
}

DEFUN ("sqlite-execute", Fsqlite_execute, Ssqlite_execute, 2, 3, 0,
       doc: /* Execute a non-select SQL statement.
If VALUES is non-nil, it should be a list of values to bind when
executing a statement like

   insert into foo values (?, ?, ...)

QUERY can include several statements, separated by a semicolon.

The number of affected rows is returned.  */)
  (Lisp_Object db, Lisp_Object query, Lisp_Object values)
{
  CHECK_SQLITE (db);
  CHECK_STRING (query);

  sqlite3 *sdb = XSQLITE (db)->db;
  char *sql, *tail;
  Lisp_Object retval = Qnil;
  const char *errmsg = NULL;

  char *top = xmalloc (SBYTES (query) + 1);
  if (top == NULL)
    return Qnil;

  memcpy (top, SSDATA (query), SBYTES (query) + 1);
  tail = top;

  while (*(sql = tail) != '\0')
    {
      sqlite3_stmt *stmt = NULL;
      int ret = sqlite3_prepare_v2 (sdb, sql, -1, &stmt, (const char**)&tail);
      /* FIXME: Same values for each statement? */
      if (!NILP (values)) {
	const char *err = bind_values (sdb, stmt, values);
	if (err != NULL)
	  {
	    errmsg = err;
	    goto exit;
	  }
      }

      if (ret != SQLITE_OK)
	{
	  if (stmt != NULL)
	    {
	      sqlite3_finalize (stmt);
	      sqlite3_reset (stmt);
	    }

	  errmsg = sqlite3_errmsg (sdb);
	  goto exit;
	}

      if (stmt == NULL)
	continue;

      ret = sqlite3_step (stmt);
      sqlite3_finalize (stmt);
      if (ret != SQLITE_OK && ret != SQLITE_DONE)
	{
	  errmsg = sqlite3_errmsg (sdb);
	  goto exit;
	}
    }

  retval = make_fixnum (sqlite3_changes (sdb));

 exit:
  xfree (top);

  if (errmsg != NULL)
    xsignal1 (Qerror, build_string (errmsg));

  return retval;
}

static Lisp_Object
row_to_value (sqlite3_stmt *stmt)
{
  int len = sqlite3_column_count (stmt);
  Lisp_Object values = Qnil;

  for (int i = 0; i < len; ++i)
    {
      Lisp_Object v = Qnil;

      switch (sqlite3_column_type (stmt, i))
	{
	case SQLITE_INTEGER:
	  v = make_fixnum (sqlite3_column_int64 (stmt, i));
	  break;

	case SQLITE_FLOAT:
	  v = make_float (sqlite3_column_double (stmt, i));
	  break;

	case SQLITE_BLOB:
	  v = make_string (sqlite3_column_blob (stmt, i),
			   sqlite3_column_bytes (stmt, i));
	  break;

	case SQLITE_NULL:
	  v = Qnil;
	  break;

	  /* The data in sqlite3 is utf-8, so we apparently don't have
	     to do any de/encoding.  */
	case SQLITE_TEXT:
	  v = make_string ((const char*)sqlite3_column_text (stmt, i),
			   sqlite3_column_bytes (stmt, i));
	  break;
	}

      values = Fcons (v, values);
    }

  return Freverse (values);
}

DEFUN ("sqlite-select", Fsqlite_select, Ssqlite_select, 2, 4, 0,
       doc: /* Select data from the database DB that matches QUERY.
If VALUES is non-nil, they are values that will be interpolated into a
parametrised statement.

By default, the return value is a list where the first element is a
list of column names, and the rest of the elements are the matching data.
If CURSOR is non-nil, an opaque object is returned instead that can
be queried with `sqlite-next' and other functions to get the data.  */)
  (Lisp_Object db, Lisp_Object query, Lisp_Object values, Lisp_Object cursor)
{
  CHECK_SQLITE (db);
  CHECK_STRING (query);

  sqlite3 *sdb = XSQLITE (db)->db;
  Lisp_Object retval = Qnil;
  const char *errmsg = NULL;

  sqlite3_stmt *stmt = NULL;
  int ret = sqlite3_prepare_v2 (sdb, SSDATA (query), SBYTES (query),
				&stmt, NULL);
  if (ret != SQLITE_OK)
    {
      if (stmt)
	sqlite3_finalize (stmt);

      goto exit;
    }

  if (!NILP (values))
    {
      const char *err = bind_values (sdb, stmt, values);
      if (err != NULL)
	{
	  sqlite3_finalize (stmt);
	  errmsg = err;
	  goto exit;
	}
    }

  /* Get the field names.  */
  Lisp_Object columns = Qnil;
  int count = sqlite3_column_count (stmt);
  for (int i = 0; i < count; ++i)
    columns = Fcons (build_string (sqlite3_column_name (stmt, i)), columns);

  columns = Fnreverse (columns);

  if (!NILP (cursor))
    {
      retval = make_sqlite (true, db, stmt, columns);
      goto exit;
    }

  /* Return the data directly.  */
  Lisp_Object data = Qnil;
  while ((ret = sqlite3_step (stmt)) == SQLITE_ROW)
    data = Fcons (row_to_value (stmt), data);

  retval = Fcons (columns, Fnreverse (data));
  sqlite3_finalize (stmt);

 exit:
  if (errmsg != NULL)
    xsignal1 (Qerror, build_string (errmsg));

  return retval;
}

static Lisp_Object
sqlite_exec (sqlite3 *sdb, const char *query)
{
  int ret = sqlite3_exec (sdb, query, NULL, NULL, NULL);
  if (ret != SQLITE_OK)
    return Qnil;

  return Qt;
}

DEFUN ("sqlite-transaction", Fsqlite_transaction, Ssqlite_transaction, 1, 1, 0,
       doc: /* Start a transaction in DB.  */)
  (Lisp_Object db)
{
  CHECK_SQLITE (db);
  return sqlite_exec (XSQLITE (db)->db, "begin");
}

DEFUN ("sqlite-commit", Fsqlite_commit, Ssqlite_commit, 1, 1, 0,
       doc: /* Commit a transaction in DB.  */)
  (Lisp_Object db)
{
  CHECK_SQLITE (db);
  return sqlite_exec (XSQLITE (db)->db, "commit");
}

DEFUN ("sqlite-rollback", Fsqlite_rollback, Ssqlite_rollback, 1, 1, 0,
       doc: /* Roll back a transaction in DB.  */)
  (Lisp_Object db)
{
  CHECK_SQLITE (db);
  return sqlite_exec (XSQLITE (db)->db, "rollback");
}

DEFUN ("sqlite-load-extension", Fsqlite_load_extension,
       Ssqlite_load_extension, 2, 2, 0,
       doc: /* Load a an SQlite module into DB.
MODULE should be the file name of an SQlite module .so file.  */)
  (Lisp_Object db, Lisp_Object module)
{
  CHECK_SQLITE (db);
  CHECK_STRING (module);

  sqlite3 *sdb = XSQLITE (db)->db;
  int result = sqlite3_load_extension (sdb, SSDATA (module), NULL, NULL);
  if (result ==  SQLITE_OK)
    return Qt;
  return Qnil;
}

DEFUN ("sqlite-next", Fsqlite_next, Ssqlite_next, 1, 1, 0,
       doc: /* Return the next result set from SET.  */)
  (Lisp_Object set)
{
  CHECK_SQLITE (set);
  if (!XSQLITE (set)->is_statement)
    xsignal1 (Qerror, build_string ("Invalid set object"));

  int ret = sqlite3_step (XSQLITE (set)->stmt);
  if (ret != SQLITE_ROW && ret != SQLITE_OK && ret != SQLITE_DONE)
    xsignal1 (Qerror, build_string (sqlite3_errmsg (XSQLITE (set)->db)));

  if (ret == SQLITE_DONE)
    {
      XSQLITE (set)->eof = true;
      return Qnil;
    }

  return row_to_value (XSQLITE (set)->stmt);
}

DEFUN ("sqlite-columns", Fsqlite_columns, Ssqlite_columns, 1, 1, 0,
       doc: /* Return the column names of SET.  */)
  (Lisp_Object set)
{
  CHECK_SQLITE (set);
  if (!XSQLITE (set)->is_statement)
    xsignal1 (Qerror, build_string ("Invalid set object"));

  return XSQLITE (set)->columns;
}

DEFUN ("sqlite-more-p", Fsqlite_more_p, Ssqlite_more_p, 1, 1, 0,
       doc: /* Say whether there's any further results in SET.  */)
  (Lisp_Object set)
{
  CHECK_SQLITE (set);
  if (!XSQLITE (set)->is_statement)
    xsignal1 (Qerror, build_string ("Invalid set object"));

  if (XSQLITE (set)->eof)
    return Qnil;
  else
    return Qt;
}

DEFUN ("sqlitep", Fsqlitep, Ssqlitep, 1, 1, 0,
       doc: /* Say whether OBJECT is an SQlite object.  */)
  (Lisp_Object object)
{
  return SQLITE (object)? Qt: Qnil;
}

#endif /* HAVE_SQLITE3 */

DEFUN ("sqlite-available-p", Fsqlite_available_p, Ssqlite_available_p, 0, 0, 0,
       doc: /* Return t if sqlite3 support is available in this instance of Emacs.*/)
  (void)
{
#ifdef HAVE_SQLITE3
  return Qt;
#else
  return Qnil;
#endif
}


void
syms_of_sqlite (void)
{
#ifdef HAVE_SQLITE3
  defsubr (&Ssqlite_open);
  defsubr (&Ssqlite_execute);
  defsubr (&Ssqlite_select);
  defsubr (&Ssqlite_transaction);
  defsubr (&Ssqlite_commit);
  defsubr (&Ssqlite_rollback);
  defsubr (&Ssqlite_load_extension);
  defsubr (&Ssqlite_next);
  defsubr (&Ssqlite_columns);
  defsubr (&Ssqlite_more_p);
  defsubr (&Ssqlitep);
  DEFSYM (Qsqlitep, "sqlitep");
#endif
  defsubr (&Ssqlite_available_p);
  DEFSYM (Qfalse, "false");
  DEFSYM (Qsqlite, "sqlite");
}
