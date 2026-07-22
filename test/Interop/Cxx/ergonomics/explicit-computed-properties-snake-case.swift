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

// snake_case with an uppercase acronym segment: only the accessor prefix and
// trailing punctuation are dropped -- the acronym's casing is kept as-is.
struct SnakeCaseAcronym {
  int get_http_URL() const SWIFT_COMPUTED_PROPERTY { return 42; }
  void set_http_URL(int p) SWIFT_COMPUTED_PROPERTY {}
};

// An annotated getter-shaped method with no get/set prefix has nothing to
// strip, so it keeps its original name as a read-only computed property.
struct SnakeCaseNoPrefix {
  int im_snake_case_swift_computed_property() const SWIFT_COMPUTED_PROPERTY { return 42; }
};

// Same as SnakeCaseAcronym, but the acronym segment is the entire name left
// after stripping the prefix.
struct SnakeCaseAllCapsAcronym {
  int Get_UTF_8() const SWIFT_COMPUTED_PROPERTY { return 42; }
  void Set_UTF_8(int p) SWIFT_COMPUTED_PROPERTY {}
};

// ALL_CAPS_SNAKE_CASE with more than one word after the accessor prefix:
// still just prefix-and-punctuation stripping, no renaming.
struct SnakeCaseAllCapsMultiWord {
  int GET_COMPUTED_PROPERTY() const SWIFT_COMPUTED_PROPERTY { return 42; }
  void SET_COMPUTED_PROPERTY(int p) SWIFT_COMPUTED_PROPERTY {}
};

// A trailing all-uppercase word is kept as-is, whether or not there's an
// accessor prefix to strip off first.
struct MixedCaseTrailingUpperWord {
  int foo_BAR() const SWIFT_COMPUTED_PROPERTY { return 42; }
  int get_baz_BAR() const SWIFT_COMPUTED_PROPERTY { return 42; }
  void set_baz_BAR(int p) SWIFT_COMPUTED_PROPERTY {}
  int GET_qux_BAR() const SWIFT_COMPUTED_PROPERTY { return 42; }
  void SET_qux_BAR(int p) SWIFT_COMPUTED_PROPERTY {}
};

// A *leading* all-uppercase word is kept as-is too -- including when it
// only becomes the first word once an accessor prefix is stripped off.
struct MixedCaseLeadingUpperWord {
  int FOO_bar() const SWIFT_COMPUTED_PROPERTY { return 42; }
  int get_BAZ_bar() const SWIFT_COMPUTED_PROPERTY { return 42; }
  void set_BAZ_bar(int p) SWIFT_COMPUTED_PROPERTY {}
  int GET_QUX_bar() const SWIFT_COMPUTED_PROPERTY { return 42; }
  void SET_QUX_bar(int p) SWIFT_COMPUTED_PROPERTY {}
};

// Same idea, with every letter remaining after stripping the accessor
// prefix uppercase: still no renaming, the all-caps spelling is preserved.
struct AllCapsNoPrefix {
  int FOO_BAR() const SWIFT_COMPUTED_PROPERTY { return 42; }
  int get_BAZ_QUX() const SWIFT_COMPUTED_PROPERTY { return 42; }
  void set_BAZ_QUX(int p) SWIFT_COMPUTED_PROPERTY {}
};

//--- test.swift

// CHECK:      struct SnakeCase {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'x' property")
// CHECK-NEXT:    func get_x() -> CInt
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'x' property")
// CHECK-NEXT:    mutating func set_x(_ p: CInt)
// CHECK-NEXT:    var x: CInt
// CHECK-NEXT: }

// CHECK:      struct SnakeCaseMultiWord {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'fooBar' property")
// CHECK-NEXT:    func get_foo_bar() -> CInt
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'fooBar' property")
// CHECK-NEXT:    mutating func set_foo_bar(_ p: CInt)
// CHECK-NEXT:    var fooBar: CInt
// CHECK-NEXT: }

// CHECK:      struct SnakeCaseAcronym {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'http_URL' property")
// CHECK-NEXT:    func get_http_URL() -> CInt
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'http_URL' property")
// CHECK-NEXT:    mutating func set_http_URL(_ p: CInt)
// CHECK-NEXT:    var http_URL: CInt
// CHECK-NEXT: }

// CHECK:      struct SnakeCaseNoPrefix {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'im_snake_case_swift_computed_property' property")
// CHECK-NEXT:    func im_snake_case_swift_computed_property() -> CInt
// CHECK-NEXT:    var im_snake_case_swift_computed_property: CInt { get }
// CHECK-NEXT: }

// CHECK:      struct SnakeCaseAllCapsAcronym {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'UTF_8' property")
// CHECK-NEXT:    func Get_UTF_8() -> CInt
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'UTF_8' property")
// CHECK-NEXT:    mutating func Set_UTF_8(_ p: CInt)
// CHECK-NEXT:    var UTF_8: CInt
// CHECK-NEXT: }

// CHECK:      struct SnakeCaseAllCapsMultiWord {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'COMPUTED_PROPERTY' property")
// CHECK-NEXT:    func GET_COMPUTED_PROPERTY() -> CInt
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'COMPUTED_PROPERTY' property")
// CHECK-NEXT:    mutating func SET_COMPUTED_PROPERTY(_ p: CInt)
// CHECK-NEXT:    var COMPUTED_PROPERTY: CInt
// CHECK-NEXT: }

// CHECK:      struct MixedCaseTrailingUpperWord {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'foo_BAR' property")
// CHECK-NEXT:    func foo_BAR() -> CInt
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'baz_BAR' property")
// CHECK-NEXT:    func get_baz_BAR() -> CInt
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'baz_BAR' property")
// CHECK-NEXT:    mutating func set_baz_BAR(_ p: CInt)
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'qux_BAR' property")
// CHECK-NEXT:    func GET_qux_BAR() -> CInt
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'qux_BAR' property")
// CHECK-NEXT:    mutating func SET_qux_BAR(_ p: CInt)
// CHECK-NEXT:    var foo_BAR: CInt { get }
// CHECK-NEXT:    var baz_BAR: CInt
// CHECK-NEXT:    var qux_BAR: CInt
// CHECK-NEXT: }

// CHECK:      struct MixedCaseLeadingUpperWord {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'FOO_bar' property")
// CHECK-NEXT:    func FOO_bar() -> CInt
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'BAZ_bar' property")
// CHECK-NEXT:    func get_BAZ_bar() -> CInt
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'BAZ_bar' property")
// CHECK-NEXT:    mutating func set_BAZ_bar(_ p: CInt)
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'QUX_bar' property")
// CHECK-NEXT:    func GET_QUX_bar() -> CInt
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'QUX_bar' property")
// CHECK-NEXT:    mutating func SET_QUX_bar(_ p: CInt)
// CHECK-NEXT:    var FOO_bar: CInt { get }
// CHECK-NEXT:    var BAZ_bar: CInt
// CHECK-NEXT:    var QUX_bar: CInt
// CHECK-NEXT: }

// CHECK:      struct AllCapsNoPrefix {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'FOO_BAR' property")
// CHECK-NEXT:    func FOO_BAR() -> CInt
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'BAZ_QUX' property")
// CHECK-NEXT:    func get_BAZ_QUX() -> CInt
// CHECK-NEXT:    @available(*, deprecated, message: "use the 'BAZ_QUX' property")
// CHECK-NEXT:    mutating func set_BAZ_QUX(_ p: CInt)
// CHECK-NEXT:    var FOO_BAR: CInt { get }
// CHECK-NEXT:    var BAZ_QUX: CInt
// CHECK-NEXT: }
