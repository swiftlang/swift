import Foo
class Foo {
  func foo() {}
  func _foo() {}
}

extension Foo {
  func extFoo() {}
  func _extFoo() {}
}

class _Bar : Foo {
  func bar() {}
  func _bar() {}
}

protocol Baz {
  func baz() {}
  func _baz() {}
}

struct SBaz : Baz {}

func quux() {}
func _quux() {}

enum Norf {
  case _A
  case B
}

func test001() {
  #^TOP_LEVEL_0,,_^#
}
// REQUIRES: objc_interop
// RUN: %complete-test %s -hide-none -tok=TOP_LEVEL_0  -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -allow-deprecated-dag-overlap -check-prefix=TOP_LEVEL_0
// TOP_LEVEL_0-LABEL: Results for filterText: [
// TOP_LEVEL_0-DAG: Foo
// TOP_LEVEL_0-DAG: FooStruct
// TOP_LEVEL_0-DAG: _Bar
// TOP_LEVEL_0-DAG: Baz
// TOP_LEVEL_0-DAG: quux
// TOP_LEVEL_0-DAG: _quux
// TOP_LEVEL_0: ]
// TOP_LEVEL_0-LABEL: Results for filterText: _ [
// TOP_LEVEL_0-DAG: _Bar
// TOP_LEVEL_0-DAG: _quux
// TOP_LEVEL_0-DAG: _internalTopLevelFunc()
// TOP_LEVEL_0-DAG: _InternalStruct
// TOP_LEVEL_0: ]
// RUN: %complete-test %s -hide-none -tok=TOP_LEVEL_0  -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=NEGATIVE_TOP_LEVEL_0
// NEGATIVE_TOP_LEVEL_0-LABEL: Results for filterText: [
// NEGATIVE_TOP_LEVEL_0-NOT: _internalTopLevelFunc()
// NEGATIVE_TOP_LEVEL_0-NOT: _InternalStruct
// NEGATIVE_TOP_LEVEL_0: ]

func test002(x: Foo) {
  x.#^FOO_QUALIFIED_0,,^#
}
// RUN: %complete-test %s -tok=FOO_QUALIFIED_0  -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -allow-deprecated-dag-overlap -check-prefix=FOO_QUALIFIED_0
// FOO_QUALIFIED_0: Results for filterText: [
// FOO_QUALIFIED_0-DAG:   foo()
// FOO_QUALIFIED_0-DAG:   extFoo()
// FOO_QUALIFIED_0-DAG:   _extFoo()
// FOO_QUALIFIED_0-DAG:   _foo()
// FOO_QUALIFIED_0: ]

func test003(x: _Bar) {
  x.#^BAR_QUALIFIED_0,,_^#
}
// RUN: %complete-test %s -tok=BAR_QUALIFIED_0  -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -allow-deprecated-dag-overlap -check-prefix=BAR_QUALIFIED_0
// BAR_QUALIFIED_0: Results for filterText: [
// BAR_QUALIFIED_0-DAG:   bar()
// BAR_QUALIFIED_0-DAG:   _bar()
// BAR_QUALIFIED_0-DAG:   foo()
// BAR_QUALIFIED_0-DAG:   extFoo()
// BAR_QUALIFIED_0: ]
// BAR_QUALIFIED_0: Results for filterText: _ [
// BAR_QUALIFIED_0-DAG:   _bar()
// BAR_QUALIFIED_0-DAG:   _foo()
// BAR_QUALIFIED_0-DAG:   _extFoo()
// BAR_QUALIFIED_0: ]
// RUN: %complete-test %s -tok=BAR_QUALIFIED_0  -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=NEGATIVE_BAR_QUALIFIED_0
// NEGATIVE_BAR_QUALIFIED_0: Results for filterText: [
// NEGATIVE_BAR_QUALIFIED_0-NOT:   _foo()
// NEGATIVE_BAR_QUALIFIED_0-NOT:   _extFoo()
// NEGATIVE_BAR_QUALIFIED_0: ]

func test004(x: Baz) {
  x.#^BAZ_QUALIFIED_0,,^#
}
// RUN: %complete-test %s -tok=BAZ_QUALIFIED_0  -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -allow-deprecated-dag-overlap -check-prefix=BAZ_QUALIFIED_0
// BAZ_QUALIFIED_0: Results for filterText: [
// BAZ_QUALIFIED_0-DAG:   baz()
// BAZ_QUALIFIED_0-DAG:   _baz()
// BAZ_QUALIFIED_0: ]

func test005(x: SBaz) {
  x.#^BAZ_QUALIFIED_1,,^#
}
// RUN: %complete-test %s -tok=BAZ_QUALIFIED_1  -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=BAZ_QUALIFIED_1
// BAZ_QUALIFIED_1: Results for filterText: [
// BAZ_QUALIFIED_1-DAG:   baz()
// BAZ_QUALIFIED_1: ]
// RUN: %complete-test %s -tok=BAZ_QUALIFIED_1  -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=NEGATIVE_BAZ_QUALIFIED_1
// NEGATIVE_BAZ_QUALIFIED_1: Results for filterText: [
// NEGATIVE_BAZ_QUALIFIED_1-NOT:   _baz()
// NEGATIVE_BAZ_QUALIFIED_1: ]

func test006() {
  Norf.#^NORF_QUALIFIED_0,,^#
}
// RUN: %complete-test %s -tok=NORF_QUALIFIED_0 -hide-low-priority=0 -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=NORF_QUALIFIED_0
// NORF_QUALIFIED_0: Results for filterText: [
// NORF_QUALIFIED_0-DAG:  _A
// NORF_QUALIFIED_0-DAG:  B
// NORF_QUALIFIED_0: ]


func test007() {
  #^NO_UNDERSCORES^#
}
// RUN: %complete-test %s -tok=NO_UNDERSCORES -hide-low-priority=0 -hide-underscores=2  -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=NO_UNDERSCORES
// NO_UNDERSCORES-NOT: {{^\s*}}_

func test010() {
  #^ALL_UNDERSCORES^#
}
// RUN: %complete-test %s -tok=ALL_UNDERSCORES -hide-low-priority=0 -hide-underscores=0  -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=ALL_UNDERSCORES
// ALL_UNDERSCORES: _Bar
// ALL_UNDERSCORES: _quux
// ALL_UNDERSCORES: _InternalStruct
// ALL_UNDERSCORES: _internalTopLevelFunc()
