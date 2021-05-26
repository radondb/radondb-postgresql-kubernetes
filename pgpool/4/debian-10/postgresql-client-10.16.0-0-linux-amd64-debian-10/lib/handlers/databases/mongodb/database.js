'use strict';

const _ = require('lodash');

const mongodbCore = $modules['com.qingcloud.mongodb-client'];
/**
 * MongoDB handler functions for a database.
 * @namespace handler.databases.mongodb.database
 */
class MongoDBDatabase {
  constructor(connection) {
    this.connection = connection;
    this.name = '';
  }

  /**
   * Wrapper of {@link base-functions.database.mongodb~insert insert} in a specific database
   * @function handler.databases.mongodb.database~insert
   */
  insert(collection, data, options) {
    const _options = _.defaults(options || {}, this.connection);
    return mongodbCore.insert(this.name, collection, data, _options);
  }

  /**
   * Wrapper of {@link base-functions.database.mongodb~getDocuments getDocuments} in a specific database
   * @function handler.databases.mongodb.database~get
   */
  get(collection, query, options) {
    const _options = _.defaults(options || {}, this.connection);
    return mongodbCore.getDocuments(this.name, collection, query, _options);
  }

  /**
   * Wrapper of {@link base-functions.database.mongodb~updateDocuments updateDocuments} in a specific database
   * @function handler.databases.mongodb.database~update
   */
  update(collection, query, update, options) {
    const _options = _.defaults(options || {}, this.connection);
    return mongodbCore.updateDocuments(this.name, collection, query, update, _options);
  }

  /**
   * Wrapper of {@link base-functions.database.mongodb~updateFields updateFields} in a specific database
   * @function handler.databases.mongodb.database~updateFields
   */
  updateFields(collection, query, fields, options) {
    const _options = _.defaults(options || {}, this.connection);
    return mongodbCore.updateFields(this.name, collection, query, fields, _options);
  }
}

module.exports = MongoDBDatabase;
