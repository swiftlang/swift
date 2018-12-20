// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-stdlib %s -module-name main -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

import Swift
import StdlibUnittest

protocol P {
  associatedtype A
}

fileprivate struct Foo {
  fileprivate struct Inner: P {
    fileprivate struct Innermost { }
    typealias A = Innermost
  }
}

public struct Wibble<T> { }

extension Wibble {
  fileprivate struct Inner: P {
    fileprivate struct Innermost { }
    typealias A = (Innermost, T)
  }
}

func getP_A<T: P>(_: T.Type) -> Any.Type {
  return T.A.self
}

let AssociatedTypeDemangleTests = TestSuite("AssociatedTypeDemangle")


AssociatedTypeDemangleTests.test("private types") {
  expectEqual(Foo.Inner.Innermost.self, getP_A(Foo.Inner.self))
  expectEqual((Wibble<Float>.Inner.Innermost, Float).self, getP_A(Wibble<Float>.Inner.self))
}

private protocol P2 {
  associatedtype A
}

private func getP2_A<T: P2>(_: T.Type) -> Any.Type {
  return T.A.self
}

struct Bar: P2 {
  typealias A = Int
}

class C1<T> { }

private class C2<T: P2>: C1<T.A> { }

AssociatedTypeDemangleTests.test("private protocols") {
  expectEqual("C2<Bar>", String(describing: C2<Bar>.self))
}

// rdar://problem/46853806
class C3<T: P>: P2 {
  fileprivate struct Inner<U: P> { }
  fileprivate typealias A = Inner<T>
}

extension Int: P {
  typealias A = Int
}

AssociatedTypeDemangleTests.test("generic anonymous contexts") {
  expectEqual("Inner<Int>", String(describing: getP2_A(C3<Int>.self)))
}


runAllTests()
