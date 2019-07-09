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

export function clock() {
    return Date.now() / 1000;
}

import CodeFlask from 'codeflask';

const editor = new CodeFlask('#editor', {
    language: 'js',
    lineNumbers: true
});


editor.updateCode('print "hey! " + 3 * 7;');

const run_btn = document.querySelector('#run');

run_btn.addEventListener('click', () => {
    messages = '';
    run(editor.getCode());
});