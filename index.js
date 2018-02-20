/* Imports */
const {app, Menu, Tray} = require('electron');
const path = require('path');
const exec = require('child_process').exec;
const cron = require('node-cron');
const player = require('play-sound')(opts = {});

const dirs = require('./src/directories.js');
const Config = require('./src/Config.js').Config;

const {currentHour} = require('./src/player.js');

/* UI Code */
let tray, contextMenu;

/**
 * Build a new context menu.
 * @param {Config} config - Config-instance to use informations from.
 * @returns {Menu} A Menu-Instance to be used as context menu.
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
  let config = new Config(dirs.config);
  config.save();

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
    play(config.current, currentHour());
    config.update();
  });
}

/**
 * Play a hourly.
 * @param {string} hourly - The hourly to play a sound from.
 * @param {string} hour - The hour for which the hourly sound is chosen.
 */
function play(hourly, hour) {
  player.play(path.join(dirs.hourlies, hourly, hour + '.wav'), function (err) {
    if (err) throw err;
  });
}
