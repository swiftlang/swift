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

// CHECK: struct Record {
// CHECK:   init()
// CHECK:   mutating func getX() -> CInt
// CHECK:   var x: CInt { mutating get }
// CHECK: }

// Without SWIFT_COMPUTED_PROPERTY or -cxx-interop-getters-setters-as-properties,
// a getter-shaped method stays a plain method.
struct NoAttribute {
  int getY() { return 42; }
};

// CHECK:      struct NoAttribute {
// CHECK-NOT:     var
// CHECK-NEXT:    init()
// CHECK-NEXT:    mutating func getY() -> CInt
// CHECK-NEXT: }

// Shape (no parameters), not the "set"-looking name, decides this is a getter.
struct GetterNamedLikeSetter {
  int settlementValue() const SWIFT_COMPUTED_PROPERTY { return 42; }
};

// CHECK:      struct GetterNamedLikeSetter {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'settlementValue' property")
// CHECK-NEXT:    func settlementValue() -> CInt
// CHECK-NEXT:    var settlementValue: CInt { get }
// CHECK-NEXT: }
