import('../pkg/elox').then(({
    run
}) => {
    self.addEventListener('message', source => {
        run(source);
    });
}).catch(err => {
    self.postMessage(`Could not load elox: ${err}`);
});