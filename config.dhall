let Prelude = https://prelude.dhall-lang.org/v22.0.0/package.dhall

let Config =
      { port : Natural
      , offsets :
          { latitudeOffsetDegrees : Double
          , longitudeOffsetDegrees : Double
          , timestampOffsetSeconds : Natural
          }
      , autoCacheCoordinates : List { latitude : Double, longitude : Double }
      , autoCachePeriodSeconds : Natural
      , currentTimeOffsetSeconds : Natural
      , customExternalAPIDomain : Optional Text
      }

let example
    : Config
    = { port = 8081
      , offsets =
        { latitudeOffsetDegrees = 0.5
        , longitudeOffsetDegrees = 0.5
        , timestampOffsetSeconds = 900
        }
      , currentTimeOffsetSeconds = 300
      , autoCacheCoordinates =
        [ { latitude = 37.6749, longitude = -122.4198 }
        , { latitude = -40.7128, longitude = 74.0160 }
        , { latitude = 55.751244, longitude = 37.618423 }
        , { latitude = 51.509865, longitude = -0.118092 }
        , { latitude = 35.652832, longitude = 139.839478 }
        , { latitude = -41.276825, longitude = 174.777969 }
        , { latitude = -21.009701, longitude = 55.269699 }
        , { latitude = -90.0, longitude = 180.0 }
        , { latitude = 90.0, longitude = -180.0 }
        ]
      , autoCachePeriodSeconds = 30
      , customExternalAPIDomain = None Text
      }

let validate =
      \(config : Config) ->
        let expected = { validPort = True }

        let actual =
              { validPort =
                      Prelude.Natural.lessThanEqual 1024 config.port
                  &&  Prelude.Natural.lessThanEqual config.port 65535
              }

        in  expected === actual

let _ = assert : validate example

in  example
