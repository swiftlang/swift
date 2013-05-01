// RUN: swift -i %s | FileCheck %s

protocol Fooable { static func foo() }

class B : Fooable {
  static func foo() { println("Beads?!") }
}

class D : B {
  static func foo() { println("Deeds?!") }
}

struct S : Fooable {
  static func foo() { println("Seeds?!") }
}

func classMetatype(b:B.metatype) {
  b.foo()
}

func structMetatype(s:S.metatype) {
  s.foo()
}

func archeMetatype<T:Fooable>(t:T.metatype) {
  t.foo()
}

// CHECK: Beads?
classMetatype(typeof(B()))
// CHECK: Deeds?
classMetatype(typeof(D()))

// CHECK: Seeds?
structMetatype(typeof(S()))

// CHECK: Beads?
archeMetatype(typeof(B()))
// CHECK: Deeds?
archeMetatype(typeof(D()))
// CHECK: Seeds?
archeMetatype(typeof(S()))

