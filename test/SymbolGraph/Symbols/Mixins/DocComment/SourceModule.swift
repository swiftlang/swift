// FYI: The lit commands and FileCheck statements are at the bottom of the file, to be resilient
// against changes to the doc comment format.

public protocol P {
   /// same module doc
   func something()
}

public struct NoDocOverride: Hashable, P {
   public func hash(into: inout Hasher) {}
   public func something() {}
}

public struct Override: Hashable, P {
   /// override of doc from Swift
   public func hash(into: inout Hasher) {}
   /// override of doc from same module symbol
   public func something() {}
}

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SourceModule -emit-module-path %t/SourceModule.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name SourceModule -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/SourceModule.symbols.json --check-prefix=NO-OVERRIDE-SAME
// RUN: %FileCheck %s --input-file %t/SourceModule.symbols.json --check-prefix=NO-OVERRIDE-OTHER
// RUN: %FileCheck %s --input-file %t/SourceModule.symbols.json --check-prefix=OVERRIDE-SAME
// RUN: %FileCheck %s --input-file %t/SourceModule.symbols.json --check-prefix=OVERRIDE-OTHER

// NO-OVERRIDE-SAME-LABEL: "precise": "s:12SourceModule13NoDocOverrideV9somethingyyF"
// NO-OVERRIDE-SAME:      "docComment": {
// NO-OVERRIDE-SAME-NEXT:        "uri": "file://{{.*}}SourceModule.swift",
// NO-OVERRIDE-SAME-NEXT:        "module": "SourceModule",
// NO-OVERRIDE-SAME-NEXT:        "lines": [
// NO-OVERRIDE-SAME-NEXT:          {
// NO-OVERRIDE-SAME-NEXT:            "range": {
// NO-OVERRIDE-SAME-NEXT:              "start": {
// NO-OVERRIDE-SAME-NEXT:                "line": 4,
// NO-OVERRIDE-SAME-NEXT:                "character": 7
// NO-OVERRIDE-SAME-NEXT:              },
// NO-OVERRIDE-SAME-NEXT:              "end": {
// NO-OVERRIDE-SAME-NEXT:                "line": 4,
// NO-OVERRIDE-SAME-NEXT:                "character": 22
// NO-OVERRIDE-SAME-NEXT:              }
// NO-OVERRIDE-SAME-NEXT:            },
// NO-OVERRIDE-SAME-NEXT:            "text": "same module doc"
// NO-OVERRIDE-SAME-NEXT:          }
// NO-OVERRIDE-SAME-NEXT:        ]
// NO-OVERRIDE-SAME-NEXT:      }

// NO-OVERRIDE-OTHER-LABEL: "precise": "s:12SourceModule13NoDocOverrideV4hash4intoys6HasherVz_tF"
// NO-OVERRIDE-OTHER:      "docComment": {
// NO-OVERRIDE-OTHER-NEXT:        "module": "Swift",
// NO-OVERRIDE-OTHER-NEXT:        "lines": [
// actual doc comment lines skipped because they're from the stdlib

// OVERRIDE-SAME-LABEL: "precise": "s:12SourceModule8OverrideV9somethingyyF",
// OVERRIDE-SAME:      "docComment": {
// OVERRIDE-SAME-NEXT:        "uri": "file://{{.*}}SourceModule.swift",
// OVERRIDE-SAME-NEXT:        "module": "SourceModule",
// OVERRIDE-SAME-NEXT:        "lines": [
// OVERRIDE-SAME-NEXT:          {
// OVERRIDE-SAME-NEXT:            "range": {
// OVERRIDE-SAME-NEXT:              "start": {
// OVERRIDE-SAME-NEXT:                "line": 16,
// OVERRIDE-SAME-NEXT:                "character": 7
// OVERRIDE-SAME-NEXT:              },
// OVERRIDE-SAME-NEXT:              "end": {
// OVERRIDE-SAME-NEXT:                "line": 16,
// OVERRIDE-SAME-NEXT:                "character": 46
// OVERRIDE-SAME-NEXT:              }
// OVERRIDE-SAME-NEXT:            },
// OVERRIDE-SAME-NEXT:            "text": "override of doc from same module symbol"
// OVERRIDE-SAME-NEXT:          }
// OVERRIDE-SAME-NEXT:        ]
// OVERRIDE-SAME-NEXT:      }

// OVERRIDE-OTHER-LABEL: "precise": "s:12SourceModule8OverrideV4hash4intoys6HasherVz_tF",
// OVERRIDE-OTHER:      "docComment": {
// OVERRIDE-OTHER-NEXT:        "uri": "file://{{.*}}SourceModule.swift",
// OVERRIDE-OTHER-NEXT:        "module": "SourceModule",
// OVERRIDE-OTHER-NEXT:        "lines": [
// OVERRIDE-OTHER-NEXT:          {
// OVERRIDE-OTHER-NEXT:            "range": {
// OVERRIDE-OTHER-NEXT:              "start": {
// OVERRIDE-OTHER-NEXT:                "line": 14,
// OVERRIDE-OTHER-NEXT:                "character": 7
// OVERRIDE-OTHER-NEXT:              },
// OVERRIDE-OTHER-NEXT:              "end": {
// OVERRIDE-OTHER-NEXT:                "line": 14,
// OVERRIDE-OTHER-NEXT:                "character": 33
// OVERRIDE-OTHER-NEXT:              }
// OVERRIDE-OTHER-NEXT:            },
// OVERRIDE-OTHER-NEXT:            "text": "override of doc from Swift"
// OVERRIDE-OTHER-NEXT:          }
// OVERRIDE-OTHER-NEXT:        ]
// OVERRIDE-OTHER-NEXT:      },
