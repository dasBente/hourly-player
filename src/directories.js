const path = require('path');

let root = path.resolve(__dirname).split(path.sep).reverse().slice(1).reverse().join(path.sep);
let resources = path.join(root, 'resources');

module.exports = {
  root: root,
  resources: resources,
  config: path.join(resources, 'config.json'),
  icon:path.join(resources, 'icon.png'),
  hourlies: path.join(resources, 'hourlies'),
  lists: path.join(resources, 'lists'),
};
