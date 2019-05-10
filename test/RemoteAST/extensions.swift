// RUN: %target-swift-remoteast-test %s | %FileCheck %s

// REQUIRES: swift-remoteast-test

@_silgen_name("printMetadataType")
func printType(_: Any.Type)

@_silgen_name("stopRemoteAST")
func stopRemoteAST()

extension Int {
  struct Inner { }
}

// CHECK: Int.Inner
printType(Int.Inner.self)

extension Int.Inner {
  struct MoreInner { }
}

// CHECK: Int.Inner.MoreInner
printType(Int.Inner.MoreInner.self)

protocol P {
  associatedtype Assoc
}

struct A<T: P, U: P> { }

extension Int: P {
  typealias Assoc = Double
}

extension String: P {
  typealias Assoc = Double
}

extension A where T.Assoc == U.Assoc {
  struct ViaSameType { }
}

// CHECK: A<Int, String>.ViaSameType
printType(A<Int, String>.ViaSameType.self)

protocol Q { }
extension Int: Q { }

extension A where T: Q {
  struct ViaProtocolConformance { }
}

// CHECK: A<Int, String>.ViaProtocolConformance
printType(A<Int, String>.ViaProtocolConformance.self)

class B { }

class C: B { }

extension B: P {
  typealias Assoc = Int
}

extension A where U: B {
  struct ViaBaseClass { }
}

// CHECK: A<Int, B>.ViaBaseClass
printType(A<Int, B>.ViaBaseClass.self)

// CHECK: A<Int, C>.ViaBaseClass
printType(A<Int, C>.ViaBaseClass.self)

extension A where T: AnyObject {
  struct ViaAnyObject {
    func wobble() {}
  }
}

// CHECK: A<C, Int>.ViaAnyObject
printType(A<C, Int>.ViaAnyObject.self)

stopRemoteAST()