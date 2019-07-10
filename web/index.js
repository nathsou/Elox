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
      if (array.get(i) == val) return i;
    }
  
    return nil;
}
  
class HashMap {
    init() {
        this.keys = Array();
        this.values = Array();
    }

    set(key, val) {
        var idx = indexOf(this.keys, key);
        if (idx == nil) {
            this.keys.push(key);
            this.values.push(val);
        } else {
            this.keys.set(idx, key);
            this.values.set(idx, val);
        }
    }

    get(key) {
        var idx = indexOf(this.keys, key);
        if (idx == nil) return nil;
        return this.values.get(idx);
    }

}

var map = HashMap();

map.set("alice", 20);
map.set("bob", 18);
map.set("charlie", "hoy!");
map.set("dan", "hey!");
map.set("bob", true);

for (var i = 0; i < map.keys.length(); i = i + 1) {
    var key = map.keys.get(i);
    print key + " -> " + map.get(key);
}

  
  `);

const run_btn = document.querySelector('#run');

run_btn.addEventListener('click', () => {
    messages = '';
    run(editor.getCode());
});