'use strict';

const _ = require('lodash');

const postgresqlCore = $modules['com.qingcloud.postgresql-client'];

/**
 * PostgreSQL handler functions for a user.
 * @namespace handler.databases.postgresql.user
 */
class PostgresqlUser {
  constructor(connection) {
    this.connection = connection;
    this.name = '';
  }

  /**
   * Wrapper of {@link base-functions.database.postgresql~grantPrivileges grantPrivileges} for a specific user
   * @function handler.databases.postgresql.user~grantPrivileges
   * @example
   * handler.user('user').grantPrivileges('mydb')
   */
  grantPrivileges(database) {
    return postgresqlCore.grantPrivileges(database, this.name, this.connection);
  }

  /**
   * Wrapper of {@link base-functions.database.postgresql~revokePrileges revokePrileges} for a specific user
   * @function handler.databases.postgresql.user~revokePrileges
   * @example
   * handler.user('user').revokePrivileges('testDB', 'user')
   */
  revokePrileges(database) {
    return postgresqlCore.revokePrivileges(database, this.name, this.connection);
  }

  /**
   * Wrapper of {@link base-functions.database.postgresql~createUser createUser} for a specific user
   * @function handler.databases.postgresql.user~create
   * @example
   * handler.user('user').create()
   */
  create(options) {
    return postgresqlCore.createUser(this.name, _.assign(options, this.connection));
  }

  /**
   * Wrapper of {@link base-functions.database.postgresql~dropUser dropUser} for a specific user
   * @function handler.databases.postgresql.user~drop
   * @example
   * handler.user('user').drop()
   */
  drop() {
    return postgresqlCore.dropUser(this.name, this.connection);
  }
}

module.exports = PostgresqlUser;
