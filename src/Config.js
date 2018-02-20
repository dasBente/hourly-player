/**
 * Contains code concerning the management of a config file
 */

const fs = require('fs');
const moment = require('moment');
const {today, hourliesFromList} = require('./player.js');

class Config {
  constructor(source) {
    const conf = readConfig(source) || {};
    let t = today();
    
    this.source = source;
    this.list = conf.list? conf.list : '';
    this.update(conf);
    this.toggleMute(conf.mute);
  }

  save(source) {
    let src = source || this.source;
    fs.writeFileSync(src, this.toString());
  }

  setHourly(hourly) {
    if (hourly) {
      this.current = hourly;
    } else {
      let list = hourliesFromList(this.list);
      this.current = list[Math.floor(Math.random() * list.length)];
    }

    this.today = today();
  }

  toggleMute(value) {
    if (value === undefined) {
      this.mute === '0'? this.mute = '1' : this.mute = '0';
    } else {
      this.mute = value;
    }
  }

  update(config) {
    let t = today();

    if (!config && (this.today !== t || !this.current) || !config.today || !config.current) {
      this.setHourly();
    } else {
      this.current = config.current;
      this.today = config.today;
    }
  }

  toJSON() {
    return {
      current: this.current,
      today: this.today,
      mute: this.mute,
      list: this.list
    };
  }
  
  toString() {
    return JSON.stringify(this.toJSON());
  }
}

/**
 * Reads a given .JSON file and parses it if possible
 */
function readConfig(confpath) {
  let res;

  try {
    res = fs.readFileSync(confpath, 'utf8');
    res = JSON.parse(res);
  } catch (err) {
    console.log(`Caught ${err}`);
    res = undefined;
  }
  
  return res;
}

module.exports.Config = Config;
