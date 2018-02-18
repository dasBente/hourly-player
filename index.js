const {app, Menu, Tray} = require('electron');
const path = require('path');
const exec = require('child_process').exec;
const fs = require('fs');

let tray;

function readConfig() {
  let res;

  try {
    res = fs.readFileSync('./resources/config.json', 'utf8');
    res = JSON.parse(res);
  } catch (err) {
    console.log(`Caught {err}`);
    res = undefined;
  }
  
  return res;
}

function writeConfig(config) {
  fs.writeFile('./resources/config.json', JSON.stringify(config), {
    encoding: 'utf8',
  }, (err) => {throw err;});
}

function play() {
  let currentHourly = 'Shigure', currentHour = '00';
  exec(`sh ./play-hourly ${currentHourly} ${currentHour}`, (err, stdout, stderr) => {
    console.log(`${stdout}`);
    console.log(`${stderr}`);
    
    if (error !== null) {
      console.log(`exec error: ${err}`);
    }
  });
}

let contextMenu;

function buildContextMenu() {
  return Menu.buildFromTemplate([
    {label: 'Shigure', click: play}
  ]);
}

app.on('ready', () => {
  tray = new Tray('./resources/icon.png');
  contextMenu = buildContextMenu();
  tray.setContextMenu(contextMenu);

  tray.on('click', () => {
    play();
  });

});

