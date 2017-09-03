const { FuseBox, SassPlugin, CSSPlugin } = require("fuse-box");
const { Sparky } = require("fsbx");

let fuse = FuseBox.init({
        homeDir: "static",
        output: "dist/static/$name.js",
        tsConfig: "tsconfig.json",
        sourceMaps: { inline: false, sourceRoot: "/sources" },
        shim: {
            jquery: {
                source: "./static/lib/jquery/jquery-3.2.1.min.js",
                exports: "$"
            },
            moment: {
                source: "../node_modules/moment/moment.js",
                exports: "moment"
            },
            daterangepicker: {
                source: "./static/lib/bootstrap/js/daterangepicker.js",
                exports: "daterangepicker"
            }
        }
    });

Sparky.task('app', () => {
    fuse.bundle("index").plugin(SassPlugin(), CSSPlugin()).instructions(`>./index.ts`);
});

Sparky.task("copy", () => {
    return Sparky.src(["static/**/**.js", 
                       "static/**/**.html",
                       "static/**/**.ttf",
                       "static/**/**.woff",
                       "static/**/**.woff2",
                       "static/**/**.ico",
                       "static/**/**.png",
                       "static/**/**.jpg"
                    ])
        .clean("dist/")
        .dest("./dist")
});

Sparky.task('default', ['app', 'copy'], () => {
    return fuse.run();
});