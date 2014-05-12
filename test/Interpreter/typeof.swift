// RUN: %target-run-simple-swift | FileCheck %s

protocol Fooable { class func foo() }

class B : Fooable {
  class func foo() { println("Beads?!") }
}

class D : B {
  override class func foo() { println("Deeds?!") }
}

struct S : Fooable {
  static func foo() { println("Seeds?!") }
}

func classMetatype(b: B.Type) {
  b.foo()
}

func structMetatype(s: S.Type) {
  s.foo()
}

func archeMetatype<T : Fooable>(t: T.Type) {
  t.foo()
}

func archeMetatype2<T : Fooable>(t: T) {
  t.dynamicType.foo()
}

// CHECK: Beads?
classMetatype(B().dynamicType)
// CHECK: Deeds?
classMetatype(D().dynamicType)

// CHECK: Seeds?
structMetatype(S().dynamicType)

// CHECK: Beads?
archeMetatype(B().dynamicType)
// FIXME: Deeds? <rdar://problem/14620454>
archeMetatype(D().dynamicType)
// CHECK: Seeds?
archeMetatype(S().dynamicType)

// CHECK: Beads?
archeMetatype2(B())
// FIXME: Deeds? <rdar://problem/14620454>
archeMetatype2(D())
// CHECK: Seeds?
archeMetatype2(S())

