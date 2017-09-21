`use strict`
// npm run-script build -> Production
// npm start -> Development
const IS_PRODUCTION = process.env.npm_lifecycle_event === 'build';
const ELM_DEBUG = IS_PRODUCTION ? 'false' : 'true'
const Path = require('path');
const Webpack = require('webpack');
const WepackMd5Hash = require('webpack-md5-hash');
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const CleanWebpackPlugin = require('clean-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const Autoprefixer = require('autoprefixer');

// const OutputPath = Path.join(__dirname, '')

const OnlyIn = (test, thing) => {
    if (test) return thing;
}

const IfDevelopment = (thing, other) => {
    return IS_PRODUCTION ? other : thing;
}