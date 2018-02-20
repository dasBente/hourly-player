const fs = require('fs');
const path = require('path');
const moment = require('moment');

const dirs = require('./directories.js');

/** 
 * Return todays date 
 * @returns {string} Todays date in YYYY-MM-DD format 
 */
const today = () => moment().format('YYYY-MM-DD');

/** 
 * Return the current hour of the day 
 * @returns {string} Current hour in 24h Format with prepended 0 for values below 10.
 */
const currentHour = () => moment().format('HH');

/** 
 * Check if a file is a directory.
 * @param {string} source - The file to check.
 * @returns {bool} 
 */
const isDir = source => fs.lstatSync(source).isDirectory();

/** 
 * Read all filenames in a given location. 
 * @param {string} source - The directorie whose files will be read. 
 * @returns {string[]} Array of all files in source.
 */
const allFiles = source => fs.readdirSync(source);

/** 
 * Gets a list of all known hourly lists. 
 * @returns {string[]} Array of the known hourly lists.
 */
const getLists = () => allFiles(dirs.lists);

/** 
 * Gets a list of all known hourlies.
 * @returns {string[]} Array of known hourly lists
 */
const allHourlies = () => allFiles(dirs.hourlies);

/**
 * Reads a file linewise
 * @param {string} source - Path to the file to be read.
 * @returns {string[]} Array of the files lines.
 */
const readLines = source => fs.readFileSync(source).toSting().split('\n');

/** 
 * Reads all hourlies from a list.
 * @param {string} [list] - The list to read from or all hourlies if not set.
 * @returns {string[]} Array of all hourlies in the selected list.
 */
function hourliesFromList(list) {
  let complement = false;

  if(!list) {
    let a = allHourlies();
    return allHourlies();
  }

  if (list.slice(0,1) === '-') {
    complement = true;
    list = list.slice(1);
  }

  let hourlies = readLines(path.join(dirs.lists, list));
  
  if (complement) {
    hourlies = allHourlies().filter(hourly => !hourlies.includes(hourly));
  }

  return hourlies;
}

module.exports = {
  today: today,
  currentHour: currentHour,
  isDir: isDir,
  allFiles: allFiles,
  getLists: getLists,
  readLines: readLines,
  allHourlies: allHourlies,
  hourliesFromList: hourliesFromList,
};
