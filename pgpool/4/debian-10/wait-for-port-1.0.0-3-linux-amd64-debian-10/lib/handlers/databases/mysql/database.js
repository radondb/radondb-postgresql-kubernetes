'use strict';

const _ = require('lodash');

const mysqlCore = $modules['com.qingcloud.mysql-client'];

/**
 * MySQL handler functions for a database.
 * @namespace handler.databases.mysql.database
 */
class MysqlDatabase {
  constructor(connection) {
    this.connection = connection;
    this.name = '';
  }

  /**
   * Wrapper of {@link base-functions.database.mysql~execute execute} in a specific database
   * @function handler.databases.mysql.database~execute
   * @example
   * handler.database('mydb').execute('SELECT * FROM mytable')
   */
  execute(command, options) {
    const _options = _.defaults(options || {}, {database: this.name, cwd: this.cwd}, this.connection);
    return mysqlCore.execute(command, _options);
  }

  /**
   * Wrapper of {@link base-functions.database.mysql~executeSqlFile executeSqlFile} in a specific database
   * @function handler.databases.mysql.database~executeFile
   * @example
   * handler.database('mydb').executeFile('test.sql')
   */
  executeFile(file, options) {
    const _options = _.defaults(options || {}, {database: this.name, cwd: this.cwd}, this.connection);
    if (!_.startsWith(file, '/')) file = $file.join(options.cwd, file);
    return mysqlCore.executeSqlFile(file, _options);
  }

  /**
   * Wrapper of {@link base-functions.database.mysql~set set} in a specific database
   * @function handler.databases.mysql.database~set
   */
  set(table, key, value, options) {
    const _options = _.defaults(options || {}, this.connection);
    return mysqlCore.set(this.name, table, key, value, _options);
  }

  /**
   * Wrapper of {@link base-functions.database.mysql~get get} in a specific database
   * @function handler.databases.mysql.database~get
   */
  get(table, key, options) {
    const _options = _.defaults(options || {}, this.connection);
    return mysqlCore.get(this.name, table, key, _options);
  }

  /**
   * Wrapper of {@link base-functions.database.mysql~insert insert} in a specific database
   * @function handler.databases.mysql.database~insert
   */
  insert(table, data, options) {
    const _options = _.defaults(options || {}, this.connection);
    return _.map(data, (row) => mysqlCore.insert(this.name, table, row, _options));
  }

  /**
   * Wrapper of {@link base-functions.database.mysql~existsDatabase existsDatabase} for a specific database
   * @function handler.databases.mysql.database~exists
   */
  exists(options) {
    const _options = _.defaults(options || {}, this.connection);
    return mysqlCore.existsDatabase(this.name, _options);
  }
}

module.exports = MysqlDatabase;
