// XFAIL: broken_std_regex

class Foo {}
class Base {
  init() {}
  func base() {}
}
class FooBar {
  init() {}
  init(x: Foo) {}
  static func fooBar() {}
  static func fooBaz() {}
  func method() {}
  let prop: FooBar { return FooBar() }
  subscript(x: Foo) -> Foo {}
}
func test001() {
  #^TOP_LEVEL_0,fo,foo,foob,foobar^#
}
// RUN: %complete-test %s -no-fuzz -group=none -add-inner-results -tok=TOP_LEVEL_0 | %FileCheck %s -check-prefix=TOP_LEVEL_0
// TOP_LEVEL_0-LABEL: Results for filterText: fo [
// TOP_LEVEL_0-NEXT:   for
// TOP_LEVEL_0-NEXT:   Foo
// TOP_LEVEL_0-NEXT:   FooBar
// TOP_LEVEL_0: ]

// TOP_LEVEL_0-LABEL: Results for filterText: foo [
// TOP_LEVEL_0-NEXT:   Foo
// TOP_LEVEL_0-NEXT:   Foo(
// TOP_LEVEL_0-NEXT:   FooBar
// TOP_LEVEL_0-NEXT:   Foo()
// TOP_LEVEL_0-NEXT: ]

// TOP_LEVEL_0-LABEL: Results for filterText: foob [
// TOP_LEVEL_0-NEXT:   FooBar
// TOP_LEVEL_0-NEXT: ]

// TOP_LEVEL_0-LABEL: Results for filterText: foobar [
// TOP_LEVEL_0-NEXT:   FooBar
// TOP_LEVEL_0-NEXT:   FooBar.
// TOP_LEVEL_0-NEXT:   FooBar(
// TOP_LEVEL_0-NEXT:   FooBar()
// TOP_LEVEL_0-NEXT:   FooBar(x: Foo)
// TOP_LEVEL_0-NEXT:   FooBar.fooBar()
// TOP_LEVEL_0-NEXT:   FooBar.fooBaz()
// TOP_LEVEL_0: ]

func test002(abc: FooBar, abd: Base) {
  #^TOP_LEVEL_1,ab,abc,abd^#
}
// RUN: %complete-test %s -no-fuzz -group=none -add-inner-results -tok=TOP_LEVEL_1 | %FileCheck %s -check-prefix=TOP_LEVEL_1
// TOP_LEVEL_1-LABEL: Results for filterText: ab [
// TOP_LEVEL_1-NEXT:   abc
// TOP_LEVEL_1-NEXT:   abd
// TOP_LEVEL_1: ]

// TOP_LEVEL_1-LABEL: Results for filterText: abc [
// TOP_LEVEL_1-NEXT:   abc
// TOP_LEVEL_1-NEXT:   abc.
// TOP_LEVEL_1-NEXT:   abc===
// TOP_LEVEL_1-NEXT:   abc!==
// TOP_LEVEL_1-NEXT:   abc.method()
// TOP_LEVEL_1-NEXT:   abc.prop
// TOP_LEVEL_1-NEXT: ]

// TOP_LEVEL_1-LABEL: Results for filterText: abd [
// TOP_LEVEL_1-NEXT:   abd
// TOP_LEVEL_1-NEXT:   abd.
// TOP_LEVEL_1-NEXT:   abd===
// TOP_LEVEL_1-NEXT:   abd!==
// TOP_LEVEL_1-NEXT:   abd.base()
// TOP_LEVEL_1-NEXT: ]

func test003(x: FooBar) {
  x.#^FOOBAR_QUALIFIED,pro,prop,prop.^#
}
// RUN: %complete-test %s -group=none -add-inner-results -tok=FOOBAR_QUALIFIED | %FileCheck %s -check-prefix=FOOBAR_QUALIFIED
// FOOBAR_QUALIFIED-LABEL: Results for filterText: pro [
// FOOBAR_QUALIFIED-NEXT:   prop
// FOOBAR_QUALIFIED-NEXT: ]

// FOOBAR_QUALIFIED-LABEL: Results for filterText: prop [
// FOOBAR_QUALIFIED-NEXT:   prop
// FOOBAR_QUALIFIED-NEXT:   prop.
// FOOBAR_QUALIFIED:   prop.method()
// FOOBAR_QUALIFIED-NEXT:   prop.prop
// FOOBAR_QUALIFIED-NEXT: ]

// Just don't explode.  We generally expect to get a new session here.
// FOOBAR_QUALIFIED-LABEL: Results for filterText: prop. [
// FOOBAR_QUALIFIED-NEXT: ]

// RUN: %complete-test %s -group=none -no-inner-results -inner-operators -tok=FOOBAR_QUALIFIED | %FileCheck %s -check-prefix=FOOBAR_QUALIFIED_OP
// FOOBAR_QUALIFIED_OP-LABEL: Results for filterText: pro [
// FOOBAR_QUALIFIED_OP-NEXT:   prop
// FOOBAR_QUALIFIED_OP-NEXT: ]
// FOOBAR_QUALIFIED_OP-LABEL: Results for filterText: prop [
// FOOBAR_QUALIFIED_OP-NEXT:   prop
// FOOBAR_QUALIFIED_OP-NEXT:   prop.
// FOOBAR_QUALIFIED_OP: ]

// RUN: %complete-test %s -group=none -add-inner-results -no-inner-operators -tok=FOOBAR_QUALIFIED | %FileCheck %s -check-prefix=FOOBAR_QUALIFIED_NOOP
// FOOBAR_QUALIFIED_NOOP-LABEL: Results for filterText: pro [
// FOOBAR_QUALIFIED_NOOP-NEXT:   prop
// FOOBAR_QUALIFIED_NOOP-NEXT: ]
// FOOBAR_QUALIFIED_NOOP-LABEL: Results for filterText: prop [
// FOOBAR_QUALIFIED_NOOP-NEXT:   prop
// FOOBAR_QUALIFIED_NOOP-NEXT:   prop.method()
// FOOBAR_QUALIFIED_NOOP-NEXT:   prop.prop
// FOOBAR_QUALIFIED_NOOP-NEXT: ]

// RUN: %complete-test %s -group=none -no-include-exact-match -add-inner-results -no-inner-operators -tok=FOOBAR_QUALIFIED | %FileCheck %s -check-prefix=FOOBAR_QUALIFIED_NOEXACT
// FOOBAR_QUALIFIED_NOEXACT-LABEL: Results for filterText: prop [
// FOOBAR_QUALIFIED_NOEXACT-NEXT:   prop.method()
// FOOBAR_QUALIFIED_NOEXACT-NEXT:   prop.prop
// FOOBAR_QUALIFIED_NOEXACT-NEXT: ]

func test004() {
  FooBar#^FOOBAR_POSTFIX^#
}
// RUN: %complete-test %s -group=none -no-inner-results -inner-operators -tok=FOOBAR_POSTFIX | %FileCheck %s -check-prefix=FOOBAR_POSTFIX_OP
// FOOBAR_POSTFIX_OP: {{^}}.{{$}}
// FOOBAR_POSTFIX_OP: {{^}}({{$}}

func test005(x: FooBar) {
  x#^FOOBAR_INSTANCE_POSTFIX^#
}
// RUN: %complete-test %s -group=none -no-inner-results -inner-operators -tok=FOOBAR_INSTANCE_POSTFIX | %FileCheck %s -check-prefix=FOOBAR_INSTANCE_POSTFIX_OP
// FOOBAR_INSTANCE_POSTFIX_OP: .
// FIXME: We should probably just have '[' here - rdar://22702955
// FOOBAR_INSTANCE_POSTFIX_OP: [Foo]

func test005(x: Base?) {
  x#^OPTIONAL_POSTFIX^#
}
// RUN: %complete-test %s -group=none -no-inner-results -inner-operators -tok=OPTIONAL_POSTFIX | %FileCheck %s -check-prefix=OPTIONAL_POSTFIX_OP
// OPTIONAL_POSTFIX_OP: .
// OPTIONAL_POSTFIX_OP: ?.

// RUN: %complete-test %s -group=none -no-inner-results -inner-operators -tok=KEYWORD_0 | %FileCheck %s -check-prefix=KEYWORD_0
func test006() {
  #^KEYWORD_0,for^#
}
// KEYWORD_0-NOT: for_
// KEYWORD_0-NOT: fortest
// KEYWORD_0-NOT: for.
