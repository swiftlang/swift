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

// Plain snake_case: already imported as a computed property.
struct SnakeCase {
  int get_x() const SWIFT_COMPUTED_PROPERTY { return 42; }
  void set_x(int p) SWIFT_COMPUTED_PROPERTY {}
};

// snake_case with a multi-word name.
struct SnakeCaseMultiWord {
  int get_foo_bar() const SWIFT_COMPUTED_PROPERTY { return 42; }
  void set_foo_bar(int p) SWIFT_COMPUTED_PROPERTY {}
};

// snake_case with an uppercase acronym segment (rdar://89453010): should still
// be imported as a computed property named `httpURL`.
struct SnakeCaseAcronym {
  int get_http_URL() const SWIFT_COMPUTED_PROPERTY { return 42; }
  void set_http_URL(int p) SWIFT_COMPUTED_PROPERTY {}
};

// An annotated getter-shaped method with NO get/set prefix should be imported
// as a read-only computed property named after its camelCased full name.
struct SnakeCaseNoPrefix {
  int im_snake_case_swift_computed_property() const SWIFT_COMPUTED_PROPERTY { return 42; }
};

//--- test.swift

// CHECK:      struct SnakeCase {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'x' property")
// CHECK-NEXT:    func get_x() -> Int32
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'x' property")
// CHECK-NEXT:    mutating func set_x(_ p: Int32)
// CHECK-NEXT:    var x: Int32
// CHECK-NEXT: }

// CHECK:      struct SnakeCaseMultiWord {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'fooBar' property")
// CHECK-NEXT:    func get_foo_bar() -> Int32
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'fooBar' property")
// CHECK-NEXT:    mutating func set_foo_bar(_ p: Int32)
// CHECK-NEXT:    var fooBar: Int32
// CHECK-NEXT: }

// CHECK:      struct SnakeCaseAcronym {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'httpURL' property")
// CHECK-NEXT:    func get_http_URL() -> Int32
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'httpURL' property")
// CHECK-NEXT:    mutating func set_http_URL(_ p: Int32)
// CHECK-NEXT:    var httpURL: Int32
// CHECK-NEXT: }

// CHECK:      struct SnakeCaseNoPrefix {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'imSnakeCaseSwiftComputedProperty' property")
// CHECK-NEXT:    func im_snake_case_swift_computed_property() -> Int32
// CHECK-NEXT:    var imSnakeCaseSwiftComputedProperty: Int32 { get }
// CHECK-NEXT: }
