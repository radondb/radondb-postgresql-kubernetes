'use strict';

const _ = require('lodash');

const postgresqlCore = $modules['com.qingcloud.postgresql-client'];

/**
 * PostgreSQL handler functions for a database.
 * @namespace handler.databases.postgresql.database
 */
class PostgresqlDatabase {
  constructor(connection) {
    this.connection = connection;
    this.name = '';
  }

  /**
   * Wrapper of {@link base-functions.database.postgresql~create create} in a specific database
   * @function handler.databases.postgresql.database~create
   */
  create() {
    return postgresqlCore.createDatabase(this.name, this.connection);
  }

  /**
   * Wrapper of {@link base-functions.database.postgresql~drop drop} in a specific database
   * @function handler.databases.postgresql.database~drop
   */
  drop() {
    return postgresqlCore.dropDatabase(this.name, this.connection);
  }

  /**
   * Wrapper of {@link base-functions.database.postgresql~set set} in a specific database
   * @function handler.databases.postgresql.database~set
   */
  set(table, key, value, options) {
    const _options = _.defaults(options || {}, this.connection);
    return postgresqlCore.set(this.name, table, key, value, _options);
  }

  /**
   * Wrapper of {@link base-functions.database.postgresql~execute execute} in a specific database
   * @function handler.databases.postgresql.database~execute
   * @example
   * handler.database('mydb').execute('SELECT * FROM mytable')
   */
  execute(command, options) {
    const _options = _.defaults(options || {}, {database: this.name, cwd: this.cwd}, this.connection);
    postgresqlCore.execute(command, _options);
  }

  /**
   * Wrapper of {@link base-functions.database.postgresql~get get} in a specific database
   * @function handler.databases.postgresql.database~get
   */
  get(table, key, options) {
    const _options = _.defaults(options || {}, this.connection);
    return postgresqlCore.get(this.name, table, key, _options);
  }

  /**
   * Wrapper of {@link base-functions.database.postgresql~insert insert} in a specific database
   * @function handler.databases.postgresql.database~insert
   */
  insert(table, data) {
    return _.map(data, (row) => postgresqlCore.insert(this.name, table, row, this.connection));
  }
}

module.exports = PostgresqlDatabase;
