'use strict';

const _ = require('lodash');
const utilsFunctions = require('./utils');

/**
 * Volume functions.
 * @namespace base-functions.volume
 */
function volumeFunctions(opts) {
  opts = opts || {};
  const applicationDirectories = _.pick(opts, [
    'installdir', 'confDir', 'binDir', 'dataDir', 'logsDir', 'libDir', 'persistDir']);
  const applicationName = opts.name;

  /**
   * Move a source folder to a destination recursively
   * @param  {String} src                   Source folder path to move
   * @param  {String} dst                   Destination folder path
   * @param  {Object} [options]             Options object
   * @param  {String} [options.srcRootDir]  Source parent folder path
   */
  function _recursiveMove(src, dst, options) {
    options = options || {};

    // Source parent folder is calculated if not provided
    const srcRootDir = options.srcRootDir || $file.dirname(src);
    // Destination parent folder
    const dstRootDir = $file.dirname(dst);
    // Destination relative folder
    const relativeDir = $file.relativize(src, srcRootDir);

    // If the source is a directory we will create that directory in the destination
    // and we will retrieve child files and folders.
    if ($file.isDirectory(src, {acceptLinks: false})) {
      // Create destination folder
      $file.mkdir($file.join(dstRootDir, relativeDir));
      // Calculating files avoids issues with temporary files disappearing in the middle of the process
      const childFiles = $file.glob($file.join(src, '*'));
      _.each(childFiles, (f) => _recursiveMove(f, dst, {srcRootDir}));
      // Removing source folder after all the subfiles and subfolders have been moved
      // Retrying to avoid errors with fs libraries or processes locking files
      utilsFunctions.retry(20, () => {
        // Delete source directory only if the source exists
        if ($file.exists(src)) {
          $file.delete(src);
        } else {
          $app.warn(`[recursiveMove] ${src} no longer exists. Skipping delete...`);
        }
      }, {msg: `Trying to delete ${src}...`, interval: 1});
    } else {
      // If the source is a file we will move it if it does not exists in the destination
      // Retrying to avoid errors with fs libraries or processes locking files
      utilsFunctions.retry(20, () => {
        // Move to destination only if the source exists and the destination doesn't exists
        if ($file.exists(src)) {
          if (!$file.exists($file.join(dstRootDir, relativeDir))) {
            $file.move(src, $file.join(dstRootDir, relativeDir));
          }
          if ($file.exists(src)) {
            // Delete src if still exists so we can link the file in prepareDataToPersist function
            $file.delete(src);
          }
        } else {
          $app.warn(`[recursiveMove] ${src} no longer exists. Skipping move...`);
        }
      }, {msg: `Trying to move ${src}...`, interval: 1});
    }
  }

  /**
   * Mark a directory as initialized
   * @function base-functions.volume~markAsInitialized
   * @param {string} folder - Folder to mark
   * @example
   * markAsInitialized($app.persistDir);
   * // Result => /qingcloud/app/.initialized
   */
  function markAsInitialized(folder) {
    $file.touch($file.join(folder, '.initialized'));
  }

  /**
   * Mark a directory as restored
   * @function base-functions.volume~markAsRestored
   * @param {string} folder - Folder to mark
   * @example
   * markAsRestored($app.persistDir);
   * // Result => /qingcloud/app/.restored
   */
  function markAsRestored(folder) {
    $file.touch($file.join(folder, '.restored'));
  }

  /**
   * Replace a directory or file with a link pointing to a persistent folder or file. (See example)
   * @function base-functions.volume~prepareDataToPersist
   * @param {string[]} dataToPersist - Application files or folders that requires persistence.
   * Substitutions are supported
   * @param {Object} [options]
   * @param {string} [options.persistDir='/qingcloud/appname'] - Directory to use to persist the data
   * @example
   * // Move the folders confDir, 'data' and the file importantFile.php to the persistent folder
   * // app/qingcloud.json:
   * //   {
   * //     ...
   * //     "dataToPersist": { "value": ["{{$app.confDir}}", "data", "importantFile.php"] },
   * //     "persistDir": {"value": "/qingcloud/app" },
   * //     ...
   * //   }
   * prepareDataToPersist(dataToPersist);
   * // Result => /opt/qingcloud/app/conf -> /qingcloud/app/conf
   *              /opt/qingcloud/app/data -> /qingcloud/app/data
   *              /opt/qingcloud/app/importantFile.php -> /qingcloud/app/importantFile.php
   */
  function prepareDataToPersist(dataToPersist, options) {
    options = _.defaults(options || {}, {
      persistDir: applicationDirectories.persistDir || `/qingcloud/${applicationName}`,
    });
    // Store data to persist
    _.each(dataToPersist, (d) => {
      d = $hb.renderText(d, {$app: applicationDirectories});
      if (d.match(/^\/.*/)) d = $file.relativize(d, applicationDirectories.installdir);
      const installedData = $file.join(applicationDirectories.installdir, d);
      const persistedData = $file.join(options.persistDir, d);
      if (!$file.exists(installedData)) {
        $file.mkdir(installedData); // Create the directory if it doesn't exist yet
      }
      $app.trace(`[prepareDataToPersist] Preparing ${installedData} to persist in ${persistedData}`);
      // Move default data to persisted directory
      _recursiveMove(installedData, persistedData, {srcRootDir: $file.dirname(installedData)});
      $file.link(persistedData, installedData);
    });
    this.markAsInitialized(options.persistDir);
  }

  /**
   * Replace a directory or a file with a link pointing to a persistent folder or file.
   * Counter part of prepareDataToPersist. (See example)
   * @function base-functions.volume~restorePersistedData
   * @param {string[]} dataToPersist - Application files or folders that requires persistence.
   * Substitutions are supported
   * @param {Object} [options]
   * @param {string} [options.persistDir='/qingcloud/appname'] - Directory used to persist the data
   * @example
   * // Delete confDir, 'data' and importantFile.php  and link to the persistent folder
   * // app/qingcloud.json:
   * //   {
   * //     ...
   * //     "dataToPersist": { "value": ["{{$app.confDir}}", "data", "importantFile.php"] },
   * //     "persistDir": {"value": "/qingcloud/app" },
   * //     ...
   * //   }
   *.restorePersistedData($app.dataToPersist);
   * // Result => /opt/qingcloud/app/conf -> /qingcloud/app/conf
   *              /opt/qingcloud/app/data -> /qingcloud/app/data
   *              /opt/qingcloud/app/importantFile.php -> /qingcloud/app/importantFile.php
   */
  function restorePersistedData(dataToPersist, options) {
    options = _.defaults(options || {}, {persistDir: `/qingcloud/${applicationName}`});
    _.each(dataToPersist, (d) => {
      d = $hb.renderText(d, {$app: applicationDirectories});
      if (d.match(/\/.*/)) d = $file.relativize(d, applicationDirectories.installdir);
      const installedData = $file.join(applicationDirectories.installdir, d);
      const persistedData = $file.join(options.persistDir, d);

      // Use case: A new folder has to be persisted
      if ($file.exists(installedData) && !$file.exists(persistedData)) {
        $app.trace(`[restorePersistedData] Preparing ${installedData} to persist in ${persistedData}`);
        $file.move(installedData, persistedData);
      }

      if (!$file.isLink(installedData)) {
        $app.trace(`[restorePersistedData] Restoring ${persistedData} to ${installedData}`);
        // Remove data in the installation folder and link
        $file.delete(installedData);
        $file.link(persistedData, installedData);
      }
    });
    this.markAsRestored(options.persistDir);
  }

  /**
   * Check if the persistFolder is initialized
   * @function base-functions.volume~isInitialized
   * @param {string} folder - Folder to check is has been initialized
   * @return {boolean}
   * @example
   * isInitialized('/bitnam/app')
   * // => true
   */
  function isInitialized(folder) {
    return $file.exists($file.join(folder, '.initialized'));
  }

  /**
   * Check if the persistFolder has been restored
   * @function base-functions.volume~hasBeenRestored
   * @param {string} folder - Folder to check is has been initialized
   * @return {boolean}
   * @example
   * hasBeenRestored('/qingcloud/app')
   * // => true
   */
  function hasBeenRestored(folder) {
    return $file.exists($file.join(folder, '.restored'));
  }

  return {
    prepareDataToPersist,
    restorePersistedData,
    isInitialized,
    hasBeenRestored,
    markAsInitialized,
    markAsRestored,
  };
}

module.exports = volumeFunctions;
