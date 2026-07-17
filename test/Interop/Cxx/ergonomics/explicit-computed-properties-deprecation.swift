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

// When an annotated getter/setter is turned into a computed property, the
// original methods are deprecated, pointing at the property.
struct GetterSetter {
  int get_x() const SWIFT_COMPUTED_PROPERTY { return 42; }
  void set_x(int v) SWIFT_COMPUTED_PROPERTY {}
};

struct GetterOnly {
  int get_value() const SWIFT_COMPUTED_PROPERTY { return 42; }
};

//--- test.swift

// CHECK:      struct GetterSetter {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'x' property")
// CHECK-NEXT:    func get_x() -> Int32
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'x' property")
// CHECK-NEXT:    mutating func set_x(_ v: Int32)
// CHECK-NEXT:    var x: Int32
// CHECK-NEXT: }

// CHECK:      struct GetterOnly {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'value' property")
// CHECK-NEXT:    func get_value() -> Int32
// CHECK-NEXT:    var value: Int32 { get }
// CHECK-NEXT: }
