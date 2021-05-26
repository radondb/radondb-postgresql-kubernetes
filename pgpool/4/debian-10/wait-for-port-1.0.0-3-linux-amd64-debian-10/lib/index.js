/* eslint-disable global-require */

'use strict';

/**
 * Qingcloud base functions.
 * @namespace base-functions
 */
module.exports = {
  host: require('./host'),
  network: require('./network'),
  volume: require('./volume'),
  database: require('./database'),
  log: require('./log'),
  component: require('./component'),
};
