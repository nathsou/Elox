import {
  run
} from '../pkg/elox';
import CodeFlask from 'codeflask';
import map_demo from '../demos/map.elox';
import fib_demo from '../demos/fib.elox';
import inheritance_demo from '../demos/inheritance.elox';
import primes_demo from '../demos/primes.elox';
import linked_list_demo from '../demos/linked_list.elox';
import math_demo from '../demos/math.elox';
import prng_demo from '../demos/prng.elox';

let messages = '';

const result = new CodeFlask('#result', {
  readonly: true
});

export function log(msg) {
  console.log(msg);
  messages += `${msg}\n`;
  result.updateCode(messages);
}

export function error(err, line, col) {
  log(`Error: [line ${line}:${col}]: ${err}`);
}

export function clock() {
  return Date.now() / 1000;
}

const editor = new CodeFlask('#editor', {
  language: 'js',
  lineNumbers: true
});

const demos = {
  PRNG: prng_demo,
  primes: primes_demo,
  fibonacci: fib_demo,
  map: map_demo,
  inheritance: inheritance_demo,
  linked_list: linked_list_demo,
  math: math_demo
};

async function loadDemo(uri) {
  const source = await (await fetch(uri)).text();
  editor.updateCode(source);
}

loadDemo(Object.values(demos)[0]);

const select = document.querySelector('#demo_selector');

for (const demo of Object.keys(demos)) {
  const option = document.createElement('option');
  option.textContent = demo;
  option.value = demo;
  select.appendChild(option);
}

select.addEventListener('change', () => {
  loadDemo(demos[select.value]);
});

const run_btn = document.querySelector('#run');

run_btn.addEventListener('click', () => {
  messages = '';
  result.updateCode(messages);
  run(editor.getCode());
});