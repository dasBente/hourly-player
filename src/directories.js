const path = require('path');
const {homedir} = require('os');

const hp = path.join(homedir(), '.hourlyplayer');
const root = path.resolve(__dirname).split(path.sep).reverse().slice(1).reverse().join(path.sep);
const resources = path.join(root, 'resources');

/**
 * @module directories
 */
module.exports = {
  /** Root path to the .hourlyplayer directory. */
  hourlyplayer: hp,
  
  /** Path to the default config file */
  config: path.join(hp, 'config.json'),

  /** Path to the tray icons image file */
  icon: path.join(resources, 'icon.png'),

  /** Path to the hourlies directory */
  hourlies: path.join(hp, 'hourlies'),

  /** Path to the lists directory */
  lists: path.join(hp, 'lists'),

  /** Path to the list chooser view */
  listChooserHTML: path.join(root, 'views', 'listchooser.html'),
};
