/** Copyright (C) 2013 jingtao.net
 *
 * Filename: test.cpp
 * Description: test codes of berkeley db
 * Author: Jingtao Xu <jingtaozf@gmail.com>
 * Created: 2013.08.23 11:11:48(+0800)
 * Last-Updated:
 *     Update #: 8
 *
 * Commentary:
 *
 **/

#include <string.h>
#include <db.h>
#include <time.h>

void display (DB *dbp)
{

}

int main()
{
    const char *home = "/tmp/bdb_c";
    const char *data_dir = "/tmp/bdb_c";
    const char *progname = "test";
    FILE *errfp = stderr;
    DB_ENV *dbenv;
    DB *dbp;
    int ret;
    DBT key, data;
    DBC *dbcp;
    int i = 0,j = 0;

    if ((ret = db_env_create(&dbenv, 0)) != 0) {
        fprintf(errfp, "%s: %s\n", progname, db_strerror(ret));
        return (1);
    }
    dbenv->set_errfile(dbenv, errfp);
    dbenv->set_errpfx(dbenv, "test");
    if ((ret = dbenv->set_cachesize(dbenv, 0, 64 * 1024, 0)) != 0) {
        dbenv->err(dbenv, ret, "set_cachesize");
        dbenv->close(dbenv, 0);
        return (1);
    }
    (void)dbenv->set_data_dir(dbenv, data_dir);
    if ((ret = dbenv->open(dbenv, home, DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0644)) != 0) {
        dbenv->err(dbenv, ret, "environment open: %s", home);
        dbenv->close(dbenv, 0);
        return (1);
    }
    if ((ret = db_create(&dbp, dbenv, 0)) != 0){
        fprintf(errfp, "%s: %s\n", progname, db_strerror(ret));
        return (1);
    }
    if ((ret = dbp->open(dbp, NULL, "exenv_db1.db", NULL, DB_BTREE, DB_CREATE,0644)) != 0){
        fprintf(stderr, "database open: %s\n", db_strerror(ret));
        return (1);
    }

    memset(&key, 0, sizeof(DBT));
    memset(&data, 0, sizeof(DBT));
    key.data = "fruit";
    key.size = sizeof("fruit");
    data.data = "apple";
    data.size = sizeof("apple");
    if ((ret = dbp->put(dbp, NULL, &key, &data, 0)) == 0)
        printf("db: (%d)%s: key stored.\n", key.size, (char *)key.data);
    else {
        dbp->err(dbp, ret, "DB->put");
        goto err;
    }
    key.data = "fruit1";
    key.size = sizeof("fruit2");
    data.data = "apple2";
    data.size = sizeof("apple2");
    if ((ret = dbp->put(dbp, NULL, &key, &data, 0)) == 0)
        printf("db: %s: key stored.\n", (char *)key.data);
    else {
        dbp->err(dbp, ret, "DB->put");
        goto err;
    }

    /* Acquire a cursor for the database. */
    if ((ret = dbp->cursor(dbp, NULL, &dbcp, 0)) != 0) {
        dbp->err(dbp, ret, "DB->cursor");
        goto err;
    }

    /* Initialize the key/data return pair. */
    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    /* Walk through the database and print out the key/data pairs. */
    while ((ret = dbcp->c_get(dbcp, &key, &data, DB_NEXT)) == 0)
        printf("(%d)%s : (%d)%s\n",
               (int)key.size, (char *)key.data,
               (int)data.size, (char *)data.data);
        if (ret != DB_NOTFOUND) {
            dbp->err(dbp, ret, "DBcursor->get");
            goto err;
        }
err:
    if ((ret = dbcp->c_close(dbcp)) != 0)
        dbp->err(dbp, ret, "DBcursor->close");

    if ((ret = dbp->close(dbp, 0)) != 0)
        fprintf(stderr,
                "%s: DB->close: %s\n", progname, db_strerror(ret));

    if ((ret = dbenv->close(dbenv, 0)) != 0) {
        fprintf(stderr, "DB_ENV->close: %s\n", db_strerror(ret));
        return (1);
    }
    return (0);
}

/*
 * Local Variables:
 * compile-command: "gcc test.c -o test -fPIC -ldb-4.8 && ./test"
 * End:
 *
 */
