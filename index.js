/* Imports */
const {app, Menu, Tray} = require('electron');
const path = require('path');
const exec = require('child_process').exec;
const fs = require('fs');
const moment = require('moment');

/* Constants */

// File paths
const dir = path.resolve(__dirname); // This files directory
const resDir = path.join(dir, 'resources');
const hourlyDir = path.join(resDir, 'hourlies');
const listsDir = path.join(resDir, 'lists');
const configDir = path.join(resDir, 'config.json');
const iconDir = path.join(resDir, 'icon.png');

/* UI Code */
let tray, contextMenu;

/**
 * Builds a new context menu using informations from a config file
 */
function buildContextMenu(config) {
  return Menu.buildFromTemplate([
    {
      label: config.current, 
      click: () => {play(config.current, currentHour())},
    }
  ]);
}

app.on('ready', () => {
  let config = initConfig(configDir);

  tray = new Tray(iconDir);
  tray.setContextMenu(buildContextMenu(config));

  tray.on('click', () => {
    play(config.current, currentHour());
  });

});

/* Helper functions */

/**
 * Gets todays date
 */
const today = () => moment().format('YYYY-MM-DD');

/**
 * Gets the current hour
 */
const currentHour = () => moment().format('HH');

/**
 * Generates the next hourly using the config file 
 */
function nextHourly(config) {
  return "Shigure";
}

/**
 * Determine whehter a given file is a directory or not
 */
const isDir = source => fs.lstatSync(source).isDirectory();

/**
 * Retrieves all directories at a given path
 */
function allDirs(source) {
  return fs.readdirSync(source).map(
    name => path.join(source, name)
  ).filter(isDirectory);
}

/**
 * Get array containing all hourly lists
 */
const getLists = () => allDirs(listsDir)

/**
 * Reads a file into an array of lines
 */
const readLines = source => fs.readFileSync(source).toString().split('\n');

/**
 * Returns an array containing every registered hourly
 */
const allHourlies = () => allDirs(hourlyDir);

/**
 * Returns an array of all hourlies in a given list. A prepended - will instead return the
 * complement. 
 */
function hourliesFromList(list) {
  let complement = false;

  if (list.slice(0,1) === '-') {
    complement = true;
    list = list.slice(1);
  }

  let hourlies = readLines(path.join(listsDir, list));
  
  if (complement) {
    hourlies = allHourlies().filter(hourly => !hourlies.includes(hourly));
  }

  return hourlies;
}

/**
 * Reads a given .JSON file and parses it if possible
 */
function readConfig(confpath) {
  let res;

  try {
    res = fs.readFileSync(confpath, 'utf8');
    res = JSON.parse(res);
  } catch (err) {
    console.log(`Caught ${err}`);
    res = undefined;
  }
  
  return res;
}

/**
 * Stringifies a config object and saves it to the file at path
 */
function writeConfig(confpath, config) {
  fs.writeFile(confpath, JSON.stringify(config), {
    encoding: 'utf8',
  }, (err) => {throw err;});
}

/**
 * Reads the players config file or creates a minimal required config
 */
function initConfig(confpath) {
  let config = readConfig(confpath);
  
  if (!config) {
    config = {};
  }
  
  let t = today();

  if (!config.today || config.today !== t || !config.current) {
    config.today = t;
    config.current = nextHourly(config);
  }

  if (!config.mute) {
    config.mute = '0';
  }

  return config;
}

/**
 * Toggles the configs mute property on (1) or off (0)
 */
function toggleMute(config) {
  if (config.mute && config.mute == '0') {
    config.mute = '1';
  } else {
    config.mute = '0';
  }
}

/**
 * (Currently) Relays a given hourly and hour to a script to play them using aplay
 */
function play(hourly, hour) {
  exec(`sh ./play-hourly ${hourly} ${hour}`, (err, stdout, stderr) => {
    console.log(`${stdout}`);
    console.log(`${stderr}`);
    
    if (err !== null) {
      console.log(`exec error: ${err}`);
    }
  });
}
