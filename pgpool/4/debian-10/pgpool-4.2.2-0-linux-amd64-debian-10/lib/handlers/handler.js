'use strict';

const _ = require('lodash');
const Logger = require('harpoon-logger');

/**
 * Handler functions.
 * @namespace handler
 */

class Handler {
  constructor(options) {
    options = _.defaults(options || {}, {
      logger: new Logger({prefix: 'handler', level: 'info'}),
      cwd: process.cwd(),
    });
    this.cwd = options.cwd;
    this.logger = options.logger;
    if (this.cwd === process.cwd()) {
      this.logger.warn(`Using ${process.cwd()} as working directory`);
    }
  }
}

module.exports = Handler;
