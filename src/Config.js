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

    if (!conf.today || conf.today !== t || !conf.current) {
      this.setHourly();
      this.today = t;
    }
    
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
      console.log(list.length);
      this.current = list[Math.floor(Math.random() * list.length)];
    }
  }

  toggleMute(value) {
    if (value === undefined) {
      this.mute === '0'? this.mute = '1' : this.mute = '0';
    } else {
      this.mute = value;
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
