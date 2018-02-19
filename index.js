const {app, Menu, Tray} = require('electron');
const path = require('path');
const exec = require('child_process').exec;
const fs = require('fs');
const moment = require('moment');

let tray;

function readConfig(path) {
  let res;

  try {
    res = fs.readFileSync(path, 'utf8');
    res = JSON.parse(res);
  } catch (err) {
    console.log(`Caught ${err}`);
    res = undefined;
  }
  
  return res;
}

function writeConfig(path, config) {
  fs.writeFile(path, JSON.stringify(config), {
    encoding: 'utf8',
  }, (err) => {throw err;});
}

function initConfig(path) {
  let config = readConfig(path);
  
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

function toggleMute(config) {
  if (config.mute == '0') {
    config.mute = '1';
  } else {
    config.mute = '0';
  }
}

function play(hourly, hour) {
  exec(`sh ./play-hourly ${hourly} ${hour}`, (err, stdout, stderr) => {
    console.log(`${stdout}`);
    console.log(`${stderr}`);
    
    if (err !== null) {
      console.log(`exec error: ${err}`);
    }
  });
}

let contextMenu;

function buildContextMenu(config) {
  return Menu.buildFromTemplate([
    {
      label: config.current, 
      click: () => {play(config.current, currentHour())},
    }
  ]);
}

function today() {
  return moment().format('YYYY-MM-DD');
}

function currentHour() {
  return moment().format('HH');
}

function nextHourly(config) {
  return "Shigure";
}

/**
 * Reads the players config file or creates a minimal required config
 */

app.on('ready', () => {
  let config = initConfig('./resources/config.json');

  tray = new Tray('./resources/icon.png');
  tray.setContextMenu(buildContextMenu(config));

  tray.on('click', () => {
    play(config.current, currentHour());
  });

});
