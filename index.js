const {app, Menu, Tray} = require('electron');
const path = require('path');
const exec = require('child_process').exec;

let tray, currentHourly = "Test";

function play() {
  exec('sh ./play-hourly Shigure 00', (error, stdout, stderr) => {
    console.log(`${stdout}`);
    console.log(`${stderr}`);
    
    if (error !== null) {
      console.log(`exec error: ${error}`);
    }
  });
}

app.on('ready', () => {
  tray = new Tray('./resources/icon.png');
  
  const contextMenu = Menu.buildFromTemplate([
    {label: 'Test', click: play},
  ]);
  
  tray.setContextMenu(contextMenu);
});
