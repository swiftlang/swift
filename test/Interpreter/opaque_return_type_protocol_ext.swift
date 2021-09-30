// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
protocol P {
  associatedtype AT
  func foo() -> AT
}

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
struct Adapter<T: P>: P {
  var inner: T

  @available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
  func foo() -> some P {
    return inner
  }
}

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
extension P {
  @available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
  func foo() -> some P {
    return Adapter(inner: self)
  }
}

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
func getPAT<T: P>(_: T.Type) -> Any.Type {
  return T.AT.self
}

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
extension Int: P { }

// CHECK: {{Adapter<Int>|too old}}
if #available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *) {
  print(getPAT(Int.self))
} else {
  print("i'm getting too old for this sh")
}

// rdar://problem/54084733

protocol Q {
  associatedtype A
  func f() -> A
}
struct X: Q {
  typealias A = Array<Int>
  func f() -> A {
    return [1, 2, 3]
  }
}
@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
dynamic func g() -> some Q {
  return X()
}

func h<T: Q>(x: T) -> (T.A?, T.A?) {
  return (.some(x.f()), .some(x.f()))
}

if #available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *) {
  let x = g()
  // CHECK: {{X()|no really}}
  print(x)
  // CHECK: {{[1, 2, 3]|too old}}
  let y = x.f()
  print(y)
  // CHECK: {{[1, 2, 3]|too old}}
  let z = h(x: x)
  print(z)
} else {
  print("no really")
  print("i'm getting way too old for this sh")
  print("way too old")
}

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
dynamic func opaqueAssocTypeUnderlyingType() -> some Any {
  return g().f()
}

extension Optional: Q where Wrapped: Q {
  func f() -> Wrapped.A? {
    return map { $0.f() }
  }
}

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
dynamic func structuralOpaqueAssocTypeUnderlyingType() -> some Any {
  return Optional(g()).f()
}

if #available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *) {
  // CHECK: {{[\1, 2, 3\]|too old}}
  let x = opaqueAssocTypeUnderlyingType()
  print(x)
  // CHECK: {{Optional\(\[1, 2, 3\]\)|too damn old}}
  let y = structuralOpaqueAssocTypeUnderlyingType()
  print(y)
} else {
  print("nope, still too old")
  print("too damn old")
}
