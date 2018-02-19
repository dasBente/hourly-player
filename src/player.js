const fs = require('fs');
const path = require('path');
const moment = require('moment');

const dirs = require('./directories.js');

/* Return the date of the current day (in year-month-day format) */
const today = () => moment().format('YYYY-MM-DD');

/* Return the current hour of the day */
const currentHour = () => moment().format('HH');

/* Checks if the file at source is a directory */
const isDir = source => fs.lstatSync(source).isDirectory();

/* Returns a list of all files in a given directory source */
const allFiles = source => fs.readdirSync(source).map(name => path.join(source, name));

/* Returns all directories within a given directory source */
const allDirs = source => allFiles(source).filter(isDir);

/* Returns a array of all list-files in the lists directory */
const getLists = () => allFiles(dirs.lists);

/* Reads all lines from a given file source into a array of these lines */
const readLines = source => fs.readFileSync(source).toSting().split('\n');

/* Returns a list of all hourlies within the hourlies directory */
const allHourlies = () => allDirs(dirs.hourlies);

/* Returns all hourlies on a given list or all hourlies if the list parameter is empty */
function hourliesFromList(list) {
    let complement = false;

  if(!list) {
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
  allDirs: allDirs,
  getLists: getLists,
  readLines: readLines,
  allHourlies: allHourlies,
  hourliesFromList: hourliesFromList,
};
