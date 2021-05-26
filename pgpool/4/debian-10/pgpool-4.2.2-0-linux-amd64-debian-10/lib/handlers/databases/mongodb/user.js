'use strict';

const mongodbCore = $modules['com.qingcloud.mongodb-client'];
/**
 * MongoDB handler functions for a user.
 * @namespace handler.databases.mongodb.user
 */
class MongoDBUser {
  constructor(connection) {
    this.connection = connection;
    this.name = '';
  }

  /**
   * Wrapper of {@link base-functions.database.mongodb~createUser createUser} in a specific database
   * @function handler.databases.mongodb.user~create
   */
  create(password, database, roles) {
    return mongodbCore.createUser(this.name, password, database, roles, this.connection);
  }

  /**
   * Wrapper of {@link base-functions.database.mongodb~dropUser dropUser} in a specific database
   * @function handler.databases.mongodb.user~drop
   */
  drop(database) {
    return mongodbCore.createUser(this.name, database, this.connection);
  }

  /**
   * Wrapper of {@link base-functions.database.mongodb~grantRoles grantRoles} in a specific database
   * @function handler.databases.mongodb.user~grantRoles
   */
  grantRoles(database, roles) {
    return mongodbCore.grantRoles(this.name, database, roles, this.connection);
  }
}

module.exports = MongoDBUser;
