'use strict';

const _ = require('lodash');
const fs = require('fs');
const utilsFunctions = require('./utils');

/**
 * Additional component functions.
 * @namespace base-functions.component
 */
function componentFunctions(opts) {
  opts = opts || {};
  const volumeFunctions = require('./volume')(opts); // eslint-disable-line global-require
  const applicationDirectories = _.pick(opts, [
    'installdir', 'confDir', 'binDir', 'dataDir', 'logsDir', 'libDir', 'persistDir']);
  const applicationName = opts.name;

  /**
   * Replace a directory with a link pointing to its standard name. (See example)
   * @function base-functions.component~normalizeDirectories
   * @param {Object} directoriesToNormalize - Key-value defining directories to be normalized
   * with key (directory identifier) value (real directory name)
   * @example
   * // Set the folder 'config' as confDir
   * // app/qingcloud.json:
   * //   {
   * //     ...
   * //     "applicationDirectories": { "value": { "confDir": "config"}},
   * //     ...
   * //   }
   * normalizeDirectories($app.applicationDirectories);
   * // Result => /opt/qingcloud/app/conf
   * //           /opt/qingcloud/app/config -> /qingcloud/app/conf
   */
  function normalizeDirectories(directoriesToNormalize) {
    // Normalize folder structure
    if (!_.isUndefined(directoriesToNormalize)) {
      _.each(directoriesToNormalize, (value, key) => {
        $file.delete(applicationDirectories[key]);
        const originalDir = value.match(/^\/.*/)
          ? value
          : $file.join(applicationDirectories.installdir, value); // Force absolute path
        $file.mkdir(originalDir);
        $file.rename(originalDir, applicationDirectories[key]);
        $file.link(applicationDirectories[key], originalDir);
      });
    }
  }

  /**
   * Print module properties passed as parameters in a fancy way
   * @function base-functions.component~printProperties
   * @param {Object} properties - Properties to pring
   * with key (printable id) value (value of the property)
   * @example
   * printProperties({'Username': $app.username, 'Password': $app.password});
   * // Result =>
   *                 ########################################################################
   *                  Installation parameters for Wordpress:
   *                    Username: 'user'
   *                    Password: '**********'
   *                  (Passwords are not shown for security reasons)
   *                 ########################################################################
   */
  function printProperties(properties) {
    let containsPasswords = false;

    $app.info('');
    $app.info('########################################################################');
    $app.info(` Installation parameters for ${applicationName}:`);
    if (
      !_.isEmpty(applicationDirectories.persistDir)
      && volumeFunctions.hasBeenRestored(applicationDirectories.persistDir)
    ) {
      $app.info('   Persisted data and properties have been restored.');
      $app.info('   Any input specified will not take effect.');
    } else {
      _.forEach(properties, function(value, id) {
        const isPassword = id.toLowerCase().indexOf('password') > -1;
        let _value = '';

        // If any password is found, flag it
        if (isPassword) {
          containsPasswords = true;
        }

        if (_.isUndefined(value)) {
          throw new Error(`Requested property '${id}' is undefined`);
        } else if (isPassword) {
          if (_.isEmpty(value)) {
            _value = 'Not set during installation';
          } else {
            _value = '**********';
          }
        } else {
          _value = value;
        }
        if (!_.isEmpty(value) || isPassword) {
          $app.info(`   ${id}: ${_value}`);
        }
      });
    }

    // Only report password message if any password is found
    if (containsPasswords) {
      $app.info(' (Passwords are not shown for security reasons)');
    } else {
      $app.info(' This installation requires no credentials.');
    }

    $app.info('########################################################################');
    $app.info('');
  }

  /**
   * Configure permissions for an array of files or folders
   * @function base-functions.component~configurePermissions
   * @param {string[]} files - List of files or folders to apply changes. It can be a string or an array
   * @param {Object} permissions - Object defining the permissions to apply by default
   * @param {string} permissions.user - System user to set as owner
   * @param {string} permissions.group - System group to set as group owner
   * @param {Object|string} permissions.mod - File mod to use like in $file.chmod
   * @param {Object} options - Options to forward to $file.chmod and $file.chown (recursive by default)
   * @param {boolean} options.recursive - Change permissions recursively
   * @param {boolean} options.followInnerSymLinks - Change permissions in the symlinks' target
   * @example
   * // Configure permissions and owner for the folders 'var' and 'pub'
   * configurePermissions(['var', 'pub'], {
   *  user: 'daemon',
   *  group: 'daemon',
   *  mod: {directory: '2770', file: '660'}
   * });
   * @example
   * // Configure different permissions and owner for the folders 'var' and 'config.php' at a time
   * configurePermissions([{
   *   path: 'var', user: 'daemon', group: 'daemon', mod: {directory: '2770', file: '660'}
   * }, {
   *   path: 'config.php', mod: '660'
   * }], {followInnerSymLinks: true});
   */
  function configurePermissions(files, permissions, options) {
    permissions = _.defaults(permissions || {}, {
      user: null,
      group: null,
    });
    options = _.defaults(options || {}, {recursive: true});

    const isBrokenLink = (file) => {
      if ($file.isLink(file)) {
        try {
          fs.statSync(file);
        } catch (e) {
          if (e.code === 'ENOENT' || e.code === 'ENOTDIR') {
            return true;
          }
          throw e;
        }
      }
      return false;
    };

    // TODO: Revisit the whole approach
    // We will allow both chown and chmod actions
    const _configurePermissions = (f, _opts) => {
      if (!isBrokenLink(f)) {
        if (permissions.user || permissions.group) {
          $app.trace(`[configurePermissions] File to chown: ${f}`);
          $file.chown(f, permissions.user, permissions.group, _opts);
        }
        if (!_.isEmpty(permissions.mod)) {
          $app.trace(`[configurePermissions] File to chmod: ${f}`);
          $file.chmod(f, permissions.mod, _opts);
        }
      } else {
        $app.trace(`[configurePermissions] Skipping broken link: ${f}`);
      }
    };

    _.each(_.flatten([files]), (file) => {
      if (_.isPlainObject(file)) {
        // TODO: Avoid this if when revisiting the approach. It is hard to document and debug.
        // TODO: We may need to create a different function for this purpose
        if (_.isEmpty(file.path)) throw new Error(`You should define a path for ${JSON.stringify(file)}`);
        configurePermissions(file.path, file, options);
      } else {
        // Get file absolute path
        let _file = _.startsWith(file, '/') ? file : $file.join(applicationDirectories.installdir, file);

        // If the file is a link we will modify the real file
        if ($file.isLink(_file)) {
          // The link can be relative or absolute
          if (_.startsWith(fs.readlinkSync(_file), '/')) {
            _file = fs.readlinkSync(_file);
          } else {
            _file = $file.join($file.dirname(_file), fs.readlinkSync(_file));
          }
        }

        // Retrying to avoid errors with fs libraries or processes locking files
        utilsFunctions.retry(20, () => {
          // If the file is a directory we will retrieve child files. Otherwise we will process the single file
          // Calculating files avoids issues with temporary files disappearing in the middle of the process
          if ($file.isDirectory(_file) && options.recursive && options.followInnerSymLinks) {
            const childFiles = $file.glob($file.join(_file, '**'));
            $app.trace(`[configurePermissions] List of files: ${JSON.stringify(childFiles)}`);
            _.each(childFiles, (f) => _configurePermissions(f, _.omit(options, ['recursive', 'followInnerSymLinks'])));
          } else {
            _configurePermissions(_file, _.omit(options, ['followInnerSymLinks']));
          }
        }, {msg: 'Trying to configure permissions...', interval: 1});
      }
    });
  }

  function _createMonitFile(path, params) {
    if (_.isUndefined(params.pidFile) || _.isUndefined(params.service)) {
      throw new Error('You should specify the service name and the PID file in order to configure monit');
    }
    if (!$os.isInPath('nami') && _.isUndefined(params.namiPath)) {
      throw new Error(
        'nami is not in the system PATH. ',
        'You should either add it or call the function specifying the nami binary'
      );
    }
    const _params = _.defaults(params, {
      namiPath: $os.findInPath('nami'),
      // monit doesn't set the HOME var we should specify it
      home: process.env.HOME,
    });
    $hb.renderToFile($file.join(__dirname, 'templates/monit.conf.tpl'), path, _params);
  }

  function _createLogrotateFile(path, params) {
    if (_.isUndefined(params.logPath)) {
      throw new Error('You should at least specify the log path in order to configure logrotate');
    }
    const _params = _.defaults(params, {
      period: 'weekly',
      rotate: 150,
    });
    if (!_.isEmpty(params.extraOptions)) {
      _params.extraOptions = params.extraOptions.join('\n ');
    }
    $hb.renderToFile($file.join(__dirname, 'templates/logrotate.conf.tpl'), path, _params);
  }

  function _createCronFile(path, params) {
    if (_.isUndefined(params.cronJobs) || !_.isArray(params.cronJobs)) {
      throw new Error('You should specify an array of cron jobs');
    }
    const cronEntries = [];
    _.each(params.cronJobs, (cronJob) => {
      if (_.isUndefined(cronJob.command) || _.isUndefined(cronJob.cronTime)) {
        throw new Error('Cron job format not reconigzed');
      }
      const cronCommand = _.isUndefined(params.runAs) ? cronJob.command : `${params.runAs} ${cronJob.command}`;
      cronEntries.push(`${cronJob.cronTime} ${cronCommand}\n`);
    });
    $hb.renderToFile($file.join(__dirname, 'templates/cron.tpl'), path, {cronEntries});
  }

  /**
   * Create different configuration files for extra tools
   * @function base-functions.component~createExtraConfigurationFiles
   * @param {Object[]|Object} configurationProperties - List of configurations to generate
   * @param {string} configuration.type - Type of configuration. Currently supported: 'monit', 'logrotate'
   * @param {string} configuration.path - Path to configuration file
   * @param {Object} configuration.params - Configuration parameters for a specific tool
   * @param {string} configuration.params.service - (Monit only) Service to monit
   * @param {string} configuration.params.pidFile - (Monit only) PID file of the service to monit
   * @param {string} configuration.params.logPath - (Logrotate only) Path to logs to rotate
   * @param {string} [configuration.params.period='weekly'] - (Logrotate only) Rotate period
   * @param {string} [configuration.params.rotate=150] - (Logrotate only) Number of rotations to store
   * @param {Array} [configuration.params.extraOptions] - (Logrotate only) Extra options to include
   * @example
   *   componentFunctions.createExtraConfigurationFiles([
   *     {type: 'monit', path: $app.monitFile, params: {service: 'tomcat', pidFile: $app.pidFile}},
   *     {type: 'logrotate', path: $app.logrotateFile, params: {logPath: $file.join($app.logsDir, '*.log')},
   *     {type: 'cron', path: $app.cronFile, params: {
   *       runAs: 'daemon', cronJobs: [{command: 'mail', cronTime: '* * * * * *'}
   *     ]}
   *   ]);
   */
  function createExtraConfigurationFiles(configurationProperties) {
    _.each(_.flatten([configurationProperties]), (conf) => {
      switch (conf.type) {
        case 'monit':
          _createMonitFile(conf.path, conf.params);
          break;
        case 'logrotate':
          _createLogrotateFile(conf.path, conf.params);
          break;
        case 'cron':
          _createCronFile(conf.path, conf.params);
          break;
        default:
          throw new Error(`${conf.type} is not a supported configuration type`);
      }
    });
  }

  /**
  * Check that the provided passwords are valid. The criteria depends on
  * our internals decitions.
  * @param  {Object} passwordParameters Key-Value pair of passwords
  * @param  {Object} options Options object
  * @param  {boolean} options.allowEmptyPassword Rule to allow empty passwords
  * @example
  * // Validate the 'rootPassword' and 'masterRootPassword' inputs
  * validatePasswords({
  *   rootPassword: $app.rootPassword,
  *   masterRootPassword: $app.masterRootPassword
  * });
  * // Validate the 'rootPassword' and 'masterRootPassword' inputs, but allow
  * // empty passwords
  * validatePasswords({
  *   rootPassword: $app.rootPassword,
  *   masterRootPassword: $app.masterRootPassword
  * }, {allowEmptyPassword: true});
  */
  function validatePasswords(passwordParameters, options) {
    const _opts = _.defaults({}, options, {allowEmptyPassword: 'no'});
    if (_opts.allowEmptyPassword === 'no') {
      _.each(passwordParameters, (value, name) => {
        if (_.isEmpty(value)) {
          throw new Error(`The provided input "${name}" is empty.`);
        }
      });
    } else {
      _.each(passwordParameters, (value, name) => {
        if (_.isEmpty(value)) {
          $app.warn(`Allowing the "${name}" input to be empty`);
        }
      });
    }
  }

  /**
   * Check if the initialization is running as root
   * @example
   * isProcessRunningAsRoot() => true/false
   * @returns {boolean} Returns `true` if the current process is running as root, else `false`
   */
  function isProcessRunningAsRoot() {
    return process.getuid() === 0;
  }

  return {
    normalizeDirectories,
    configurePermissions,
    printProperties,
    createExtraConfigurationFiles,
    validatePasswords,
    isProcessRunningAsRoot,
  };
}

module.exports = componentFunctions;
