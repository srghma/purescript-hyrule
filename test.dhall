let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "test/**/*.purs" ]
        , dependencies = conf.dependencies # [ "aff", "spec", "profunctor", "transformers", "console", "js-date" ]
        }
