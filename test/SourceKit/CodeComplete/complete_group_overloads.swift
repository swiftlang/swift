// XFAIL: broken_std_regex

struct A {}
struct B {}

func aaa() {}
func aaa(_ x: A) {}
func aaa(_ x: B) {}
func aaa(_ x: B, y: B) {}
func aaa(x x: B, y: B) {}
func aab() {}

func test001() {
  #^TOP_LEVEL_0,aa^#
}
// RUN: %complete-test -group=overloads -tok=TOP_LEVEL_0 %s | %FileCheck -check-prefix=TOP_LEVEL_0 %s
// TOP_LEVEL_0-LABEL: aaa:
// TOP_LEVEL_0-NEXT:   aaa()
// TOP_LEVEL_0-NEXT:   aaa(x: A)
// TOP_LEVEL_0-NEXT:   aaa(x: B)
// TOP_LEVEL_0-NEXT:   aaa(x: B, y: B)
// TOP_LEVEL_0-NEXT:   aaa(x: B, y: B)
// TOP_LEVEL_0-NEXT: #colorLiteral(red: Float, green: Float, blue: Float, alpha: Float)
// TOP_LEVEL_0-NEXT: #imageLiteral(resourceName: String)
// TOP_LEVEL_0-NEXT: aab()

struct Foo {
  func aaa() {}
  func aaa(_ x: A) {}
  func aaa(_ x: B) {}
  func aaa(_ x: B, y: B) {}
  func aaa(x x: B, y: B) {}
  func aab() {}
}

func test002() {
  Foo().#^FOO_INSTANCE_0^#
}
// RUN: %complete-test -group=overloads -tok=FOO_INSTANCE_0 %s | %FileCheck -check-prefix=FOO_INSTANCE_0 %s
// FOO_INSTANCE_0-LABEL: aaa:
// FOO_INSTANCE_0-NEXT:   aaa()
// FOO_INSTANCE_0-NEXT:   aaa(x: A)
// FOO_INSTANCE_0-NEXT:   aaa(x: B)
// FOO_INSTANCE_0-NEXT:   aaa(x: B, y: B)
// FOO_INSTANCE_0-NEXT:   aaa(x: B, y: B)
// FOO_INSTANCE_0-NEXT: aab()


extension Foo {
  static func bbb() {}
  static func bbb(_ x: A) {}
  static func bbc() {}
}

func test003() {
  Foo.#^FOO_QUAL_0^#
}
// RUN: %complete-test -group=overloads -tok=FOO_QUAL_0 %s | %FileCheck -check-prefix=FOO_QUAL_0 %s
// FOO_QUAL_0-LABEL: bbb:
// FOO_QUAL_0-NEXT:   bbb()
// FOO_QUAL_0-NEXT:   bbb(x: A)
// FOO_QUAL_0-NEXT: bbc()

extension Foo {
  subscript(x: A) -> A { return A() }
  subscript(x: B) -> B { return B() }
}

func test004() {
  Foo()#^FOO_SUBSCRIPT_0^#
}
// RUN: %complete-test -group=overloads -tok=FOO_SUBSCRIPT_0 %s | %FileCheck -check-prefix=FOO_SUBSCRIPT_0 %s
// FOO_SUBSCRIPT_0-LABEL: [:
// FOO_SUBSCRIPT_0-NEXT:   [A]
// FOO_SUBSCRIPT_0-NEXT:   [B]

struct Bar {
  init() {}
  init(x: A) {}
  init(x: B) {}
}

func test005() {
  Bar#^BAR_INIT_0^#
}
// Inline a lonely group
// RUN: %complete-test -group=overloads -add-inner-results -no-inner-operators -tok=BAR_INIT_0 %s | %FileCheck -check-prefix=BAR_INIT_0 %s
// BAR_INIT_0-LABEL: (:
// BAR_INIT_0: ()
// BAR_INIT_0-NEXT: (x: A)
// BAR_INIT_0-NEXT: (x: B)
// BAR_INIT_0-NEXT: .self

extension Bar {
  func foo()
}

func test006() {
  Bar#^BAR_INIT_1^#
}
// RUN: %complete-test -group=overloads -add-inner-results -no-inner-operators -tok=BAR_INIT_1 %s | %FileCheck -check-prefix=BAR_INIT_1 %s
// BAR_INIT_1-LABEL: (:
// BAR_INIT_1-NEXT:   ()
// BAR_INIT_1-NEXT:   (x: A)
// BAR_INIT_1-NEXT:   (x: B)
// BAR_INIT_1-NEXT: foo(self: Bar)

func test007() {
  #^BAR_INIT_2^#
// RUN: %complete-test -add-inits-to-top-level -group=overloads -tok=BAR_INIT_2 %s | %FileCheck -check-prefix=BAR_INIT_2 %s
// BAR_INIT_2-LABEL: Bar:
// BAR_INIT_2-NEXT:   Bar
// BAR_INIT_2-NEXT:   Bar()
// BAR_INIT_2-NEXT:   Bar(x: A)
// BAR_INIT_2-NEXT:   Bar(x: B)
