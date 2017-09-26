import './index.html'
import './scss/main.scss'
declare function require(name:string):any;
var Elm = require("../elm/Main.elm");
var app = Elm.Main.fullscreen();