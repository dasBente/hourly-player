const path = require('path');
const {homedir} = require('os');

let root = path.join(homedir(), '.hourlyplayer');

let resources = path.join(
  path.resolve(__dirname).split(path.sep).reverse().slice(1).reverse().join(path.sep),
  'resources'
);

/**
 * @module directories
 */
module.exports = {
  /** Root path to the .hourlyplayer directory. */
  root: root,
  
  /** Path to the default config file */
  config: path.join(root, 'config.json'),

  /** Path to the tray icons image file */
  icon: path.join(resources, 'icon.png'),

  /** Path to the hourlies directory */
  hourlies: path.join(root, 'hourlies'),

  /** Path to the lists directory */
  lists: path.join(root, 'lists'),
};