import {
    run
} from '../pkg/elox';

let messages = '';

const result = new CodeFlask('#result', {
    readonly: true
});

export function log(msg) {
    console.log(msg);
    messages += `${msg}\n`;
    result.updateCode(messages);
}

export function error(err) {
    log(`Error: ${err}`);
}

export function clock() {
    return Date.now() / 1000;
}

import CodeFlask from 'codeflask';

const editor = new CodeFlask('#editor', {
    language: 'js',
    lineNumbers: true
});


editor.updateCode(`
fun indexOf(array, val) {
    var len = array.length();
    for (var i = 0; i < len; i = i + 1) {
      if (array[i] == val) return i;
    }
  
    return nil;
}
  
class HashMap {
    init() {
      this.keys = Array();
      this.values = Array();
    }
  
    #set(key, val) {
        var idx = indexOf(this.keys, key);
        if (idx == nil) {
          this.keys.push(key);
          this.values.push(val);
        } else {
          this.keys[idx] = key;
          this.values[idx] = val;
        }
    }
  
    #get(key) {
        var idx = indexOf(this.keys, key);
        if (idx == nil) return nil;
        return this.values[idx];
    }

    size() {
      return this.keys.length();
    }
  
}
  
var map = HashMap();
  
map["alice"] = 20;
map["bob"] = 18;
map["charlie"] = "hoy!";
map["dan"] = "hey!";
map["bob"] = true;
  
for (var i = 0; i < map.size(); i = i + 1) {
  var key = map.keys[i];
  print key + " -> " + map[key];
}

  `);

const run_btn = document.querySelector('#run');

run_btn.addEventListener('click', () => {
    messages = '';
    run(editor.getCode());
});