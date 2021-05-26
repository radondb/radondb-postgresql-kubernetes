'use strict';

const _ = require('lodash');

const delegate = require('harpoon-utils').delegate;

/**
 * Additional utils functions.
 * @namespace base-functions.utils
 */

/**
 * Retry a function for 'retries' attempts by catching any error it may occur.
 * @function base-functions.utils~retry
 * @param {Number} retries - Number of attempts to retry
 * @param {Function} fn - Function to try
 * @param {Object} [options] - Options object
 * @param {string} [options.msg] - Message to show when retrying. An attempts counter will be appended too.
 * @param {Number} [options.interval] - Interval (in seconds) to wait between attempts.
 * @example
 * // Retry 5 times to chown a file that it maybe not exists
 * retry(5, () => fs.chown(...), {msg: 'Trying to chown file'});
 */
function retry(retries, fn, options) {
  const _opts = _.defaults({}, options, {msg: 'Retrying...', interval: 0});
  while (retries > 0) {
    try {
      return fn();
    } catch (e) {
      $app.trace2(`[retry] Error: ${e}`);
      if (retries > 0) {
        $app.trace(`[retry] ${_opts.msg} ${retries - 1} remaining attempts`);
        $util.sleep(_opts.interval);
        retries -= 1;
      }
      if (retries === 0) throw (e);
    }
  }
  return false;
}

/**
 * Get the logger from the current aplication.
 * This function could be deprecated once the following issue is resolved at nami level
 * https://github.com/qingcloud/nami-core/issues/28
 * @function base-functions.utils~getLoggerFromApp
 * @param {Object} app - Application that contains the logger level
 * @example
 * // Get logger from app
 * getLoggerFromApp($app);
 */
function getLoggerFromApp(app) {
  const logger = {};
  const loggerRequirements = [
    'error', 'warn', 'info', 'debug', 'trace', 'trace1', 'trace2',
    'trace3', 'trace4', 'trace5', 'trace6', 'trace7', 'trace8',
  ];
  if (_.every(loggerRequirements, (r) => { return _.has(app, r); })) {
    delegate(logger, loggerRequirements, app);
  }
  return logger;
}

module.exports = {
  retry,
  getLoggerFromApp,
};
