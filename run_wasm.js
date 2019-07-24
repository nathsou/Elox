const {
    readFileSync
} = require('fs');

WebAssembly.instantiate(readFileSync('out.wasm'), {
    host: {
        print: console.log
    }
}).then(instance => {

}).catch(e => {
    console.error(e);
});