import './scss/main.scss'
declare function require(name:string):any;
declare var moment: any;
declare var $: any;
var Elm = require("../elm/Main.elm");
var app = Elm.Main.fullscreen();