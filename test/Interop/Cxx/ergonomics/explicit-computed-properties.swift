// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-ide-test -print-module -module-to-print=Test -I %t/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

//--- Inputs/module.modulemap
module Test {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h

#define SWIFT_COMPUTED_PROPERTY \
  __attribute__((swift_attr("import_computed_property")))

struct Record {
  int getX() SWIFT_COMPUTED_PROPERTY { return 42; }
};

//--- test.swift

// CHECK: struct Record {
// CHECK:   init()
// CHECK:   var x: Int32 { mutating get }
// CHECK:   mutating func getX() -> Int32
// CHECK: }
