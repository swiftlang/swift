// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %swift -target arm64-apple-macos13.0 -target-variant arm64-apple-ios16.0-macabi -typecheck -parse-as-library %t/test.swift -emit-tbd -emit-tbd-path %t/zippered.tbd -tbd-install_name Zippered 
// RUN: %llvm-readtapi --compare %t/zippered.tbd %t/expected.tbd

//--- test.swift
public func foo() -> Bool {
    return true
}

//--- expected.tbd
{
  "main_library": {
    "compatibility_versions": [
      {
        "version": "0"
      }
    ],
    "current_versions": [
      {
        "version": "0"
      }
    ],
    "exported_symbols": [
      {
        "text": {
          "global": [
            "_$s4test3fooSbyF"
          ]
        }
      }
    ],
    "flags": [
      {
        "attributes": [
          "not_app_extension_safe"
        ]
      }
    ],
    "install_names": [
      {
        "name": "Zippered"
      }
    ],
    "swift_abi": [
      {
        "abi": 7
      }
    ],
    "target_info": [
      {
        "min_deployment": "13.0",
        "target": "arm64-macos"
      },
      {
        "min_deployment": "16.0",
        "target": "arm64-maccatalyst"
      }
    ]
  },
  "tapi_tbd_version": 5
}
