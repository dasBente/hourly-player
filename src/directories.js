const path = require('path');
const {homedir} = require('os');

let root = path.join(homedir(), '.hourlyplayer');

module.exports = {
  root: root,
  config: path.join(root, 'config.json'),
  icon:path.join(root, 'icon.png'),
  hourlies: path.join(root, 'hourlies'),
  lists: path.join(root, 'lists'),
};
