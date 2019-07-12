const CopyWebpackPlugin = require("copy-webpack-plugin");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");
const path = require('path');

module.exports = {
    entry: "./bootstrap.js",
    output: {
        filename: "elox.js",
        path: path.resolve(__dirname, "dist"),
    },
    resolve: {
        extensions: ['.js', '.wasm', '.elox']
    },
    module: {
        rules: [{
            test: /\.(elox)$/i,
            use: [{
                loader: 'file-loader',
                options: {
                    name: '[name]_demo.[ext]',
                },
            }, ],
        }],
    },
    mode: "production",
    plugins: [
        new CopyWebpackPlugin(['index.html']),
        new WasmPackPlugin({
            crateDirectory: path.resolve('../')
        }),
    ],

};