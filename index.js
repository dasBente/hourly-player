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

function startSchedule(config) {
  cron.schedule('0 * * * *', function() {
    play(config.current, currentHour());
    config.update();
  });
}

/**
 * (Currently) Relays a given hourly and hour to a script to play them using aplay
 */
function play(hourly, hour) {
  player.play(path.join(dirs.hourlies, hourly, hour + '.wav'), function (err) {
    if (err) throw err;
  });
}
