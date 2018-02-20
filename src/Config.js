/**
 * Contains code concerning the management of a config file
 */

const fs = require('fs');
const moment = require('moment');
const {today, hourliesFromList} = require('./player.js');

/** Class representing a Config file */
class Config {
  /** 
   * Create a config instance.
   * @param {string} source - A path to the source config file
   */
  constructor(source) {
    const conf = readConfig(source) || {};
    let t = today();
    
    this.source = source;
    this.list = conf.list? conf.list : '';
    this.toggleMute(conf.mute);
    this.update(conf);
  }

  /**
   * Saves the config instance to a file.
   * @param {string} [source] - A path to the source to be saved to. Omitting this will use the
   *     source set in the constructor.
   */
  save(source) {
    let src = source || this.source;
    fs.writeFileSync(src, this.toString());
  }

  /**
   * Set the hourly to a new one by random or choice.
   * @param {object} [opts] - A object of possible options for the hourly generation. If unset, the
   *     hourly will just be determined at random from all hourlies saved in list this.list.
   * @param {string} [opts.hourly] - Set this.hourly to this value.
   * @param {stirng} [opts.list] - Set the list from which the hourly should be chosen.
   */
  setHourly(opts) {
    if (hourly) {
      this.current = hourly;
    } else {
      let list = hourliesFromList(this.list);
      this.current = list[Math.floor(Math.random() * list.length)];
    }

    this.today = today();
  }

  /**
   * Toggle if the app is muted.
   * @param {string} [value] - The value to set this.mute to (out of '0' and '1'). If unset will 
   *     merely toggle the already set value.
   */
  toggleMute(value) {
    if (value === undefined) {
      this.mute === '0'? this.mute = '1' : this.mute = '0';
    } else {
      this.mute = value;
    }
  }

  /**
   * Check if the application need to be updated.
   * @param {Object} [config] - Initialize with values from this config if provided instead of 
   *     regular update.
   */
  update(config) {
    let t = today();

    if (!config && (this.today !== t || !this.current) || !config.today || !config.current) {
      this.setHourly();
    } else {
      this.current = config.current;
      this.today = config.today;
    }
  }

  /**
   * Generates a object from this Config-instance.
   */
  toJSON() {
    return {
      current: this.current,
      today: this.today,
      mute: this.mute,
      list: this.list
    };
  }
  
  /**
   * Returns stringified this.toJSON() as string representation.
   */
  toString() {
    return JSON.stringify(this.toJSON());
  }
}

/**
 * Parses a config file into a JS object.
 * @param {string} confpath - Path to the config file to be parsed.
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
