'use strict';

const mysqlCore = $modules['com.qingcloud.mysql-client'];
const _ = require('lodash');

/**
 * MySQL handler functions for a user.
 * @namespace handler.databases.mysql.user
 */
class MysqlUser {
  constructor(connection) {
    this.connection = connection;
    this.name = '';
  }

  /**
   * Wrapper of {@link base-functions.database.mysql~grantPrivileges grantPrivileges} for a specific user
   * @function handler.databases.mysql.user~grantPrivileges
   * @example
   * handler.user('user').grantPrivileges('mydb')
   */
  grantPrivileges(database, options) {
    const _options = _.defaults(options || {}, this.connection);
    return mysqlCore.grantPrivileges(database, this.name, _options);
  }

  /**
   * Wrapper of {@link base-functions.database.mysql~createUser createUser} for a specific user
   * @function handler.databases.mysql.user~create
   * @example
   * handler.user('user').create()
   */
  create(options) {
    const _options = _.defaults(options || {}, this.connection);
    return mysqlCore.createUser(this.name, _options);
  }

  /**
   * Wrapper of {@link base-functions.database.mysql~dropUser dropUser} for a specific user
   * @function handler.databases.mysql.user~drop
   * @example
   * handler.user('user').drop()
   */
  drop(options) {
    const _options = _.defaults(options || {}, this.connection);
    return mysqlCore.dropUser(this.name, _options);
  }
}

module.exports = MysqlUser;
