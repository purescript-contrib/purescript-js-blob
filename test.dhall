let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "test/**/*.purs" ]
        , dependencies = conf.dependencies # [ "assert", "aff", "arrays", "js-promise", "js-promise-aff", "arraybuffer-types" ]
        }
