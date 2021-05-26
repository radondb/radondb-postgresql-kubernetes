'use strict';

const _ = require('lodash');
const os = require('os');

/**
* Host machine functions.
* @namespace base-functions.host
*/

/**
* Returns memory in Megabytes
* @function base-functions.host~sanitizeMemory
* @param {string} [memory] - Memory available (Supports M or G)
* @returns {string}
* @example
* sanitizeMemory('2G');
* // => '2048'
*/
function sanitizeMemory(_memory) {
  if (_.isString(_memory)) {
    _memory = _memory.match(/^([0-9]*)(M?G?)/);
    if (_.isNull(_memory)) throw new Error('Memory format not reconized');
    switch (_memory[2]) {
      case 'M':
        _memory = _memory[1];
        break;
      case 'G':
        _memory = +_memory[1] * 1024;
        break;
      default:
        _memory = _memory[1];
    }
  }
  return _memory;
}

/**
* Returns machine size
* @function base-functions.host~getMachineSize
* @param {Object} [options]
* @param {string} [options.memory] - Override default memory available (Supports M or G)
* @returns {string} Size: [micro|small|medium|large|xlarge|2xlarge]
* @example
* getMachineSize({memory: '2G'});
* // => 'small'
*/
function getMachineSize(options) {
  if (_.isEmpty(options.memory)) {
    // Get system available memory in Megabytes
    options.memory = os.totalmem() / (1024 * 1024);
  }
  const sizes = [
    {name: 'micro', range: {min: 0, max: 1500}},
    {name: 'small', range: {min: 1500, max: 3000}},
    {name: 'medium', range: {min: 3000, max: 6000}},
    {name: 'large', range: {min: 6000, max: 13000}},
    {name: 'xlarge', range: {min: 13000, max: 26000}},
    {name: '2xlarge', range: {min: 26000, max: Infinity}},
  ];
  let _memory = options.memory;
  _memory = sanitizeMemory(_memory);
  return _.find(sizes, (size) => _.inRange(_memory, size.range.min, size.range.max)).name;
}

module.exports = {
  sanitizeMemory,
  getMachineSize,
};
