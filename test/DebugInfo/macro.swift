// RUN: %target-swift-frontend -emit-ir %s -g -I %S/Inputs -o - \
// RUN:    -parse-as-library | %FileCheck %s

// The source file for "macro_enum", which is defined using a macro, should be
// correctly identified.

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "macro_enum",
// CHECK-SAME:             file: ![[MACRO_H:[0-9]+]]
// CHECK: ![[MACRO_H]] = !DIFile(filename: "{{.*}}{{(/|\\\\)}}Inputs{{(/|\\\\)}}Macro.h",

import Macro

public func f(_ e : macro_enum) -> Int32 {
  switch (e) {
  case zero:
    return 0
  default:
    return e.rawValue
  }
}
