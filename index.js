/* Imports */
const {app, Menu, Tray, BrowserWindow} = require('electron');
const path = require('path');
const exec = require('child_process').exec;
const cron = require('node-cron');
const player = require('play-sound')(opts = {});

const dirs = require('./src/directories.js');
const Config = require('./src/Config.js').Config;
const {getLists} = require('./src/player.js');

const {currentHour} = require('./src/player.js');

/* UI Code */
let tray, contextMenu;

/**
 * Build a menu to pick a new list to be used.
 * @param {Config} config - The config which provides the current list.
 */
function buildListChooser(config) {
  let lists = getLists();
  let window = new BrowserWindow({
    height: 200, width: 400
  });

  window.loadURL(`file://${dirs.listChooserHTML}`);
}

/**
 * Build a new context menu.
 * @param {Config} config - Config-instance to use informations from.
 * @returns {Menu} A Menu-Instance to be used as context menu.
 */
function buildContextMenu(config) {
  return Menu.buildFromTemplate([
    {
      label: config.current, 
      click: () => {play(config.current, currentHour());},
    },
    {
      label: config.list || '<Choose list>',
      click: () => {buildListChooser(config);},
    },
    {
      label: 'Mute',
      type: 'checkbox',
      checked: config.mute === '1',
      click: () => {config.toggleMute();}
    }
  ]);
}

app.on('ready', () => {
  let config = new Config(dirs.config);

  tray = new Tray(dirs.icon);
  tray.setContextMenu(buildContextMenu(config));

  tray.on('click', () => {
    play(config.current, currentHour());
  });

  console.log(config.toString());
  startSchedule(config);
});

/**
 * Run the play function at a hourly schedule
 * @param {Config} config - The config to retrieve the current hourly from.
 */
function startSchedule(config) {
  cron.schedule('0 * * * *', function() {
    if (config.mute === '0') {
      play(config.current, currentHour());
    }

    config.update();
  });
}

let playing = false; // Used to ensure that only one sound clip plays at a time

/**
 * Play a hourly.
 * @param {string} hourly - The hourly to play a sound from.
 * @param {string} hour - The hour for which the hourly sound is chosen.
 */
function play(hourly, hour) {
  if (!playing) {
    playing = true;
    
    player.play(path.join(dirs.hourlies, hourly, hour + '.wav'), function (err) {
      playing = false;
      if (err) throw err;
    });
  }
}
