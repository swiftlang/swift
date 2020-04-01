// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

protocol Protocol {}
extension Protocol {
  func foo() {
    print("I survived in \(Self.self).\(#function)")
  }
}

struct Foo<T: Equatable> {
  struct Inner1 where T: Protocol {
    let bar: T
  }

  struct Inner2 where T == String {
    func getString() -> T {
      return "I survived in \(#function), T := \(T.self)"
    }
  }

  struct Inner3<U: Hashable> {
    func isLessThan(lhs: T, rhs: T) -> U where T: Comparable, U == Bool {
      return lhs < rhs
    }

    init() {
      print("This is the unconstrained \(#function)")
    }
    init() where U == T {
      print("I survived in \(#function), T := \(T.self), U := \(U.self)")
    }
  }
}

struct ProtocolAdopter: Protocol, Equatable {}

// CHECK: I survived in ProtocolAdopter.foo()
Foo<ProtocolAdopter>.Inner1(bar: ProtocolAdopter()).bar.foo()

// CHECK: I survived in getString(), T := String
print(Foo<String>.Inner2().getString())

// CHECK: This is the unconstrained init()
// CHECK: false
print(Foo<Int>.Inner3<Bool>().isLessThan(lhs: .zero, rhs: .zero))

// CHECK: I survived in init(), T := Bool, U := Bool
_ = Foo<Bool>.Inner3<Bool>()

protocol RefinedProtocol: Protocol {
  associatedtype Assoc = Self
  associatedtype Bssoc: RefinedProtocol

  func overload()
}

extension RefinedProtocol {
  func callOverload() {
    overload()
    print("Assoc := \(Assoc.self), Bssoc := \(Bssoc.self)")
  }

  func overload() where Assoc == Bssoc {
    print("I survived in \(Self.self).\(#function) (1)")
  }
  func overload() where Assoc == Self {
    print("I survived in \(Self.self).\(#function) (2)")
  }
  func overload() where Assoc: Sequence, Bssoc == Assoc.Element {
    print("I survived in \(Self.self).\(#function) (3)")
  }
}

struct RefinedProtocolAdopter1: RefinedProtocol {
  typealias Assoc = RefinedProtocolAdopter2
  typealias Bssoc = RefinedProtocolAdopter2
}
struct RefinedProtocolAdopter2: RefinedProtocol {
  typealias Bssoc = RefinedProtocolAdopter1
}
struct RefinedProtocolAdopter3: RefinedProtocol {
  typealias Assoc = Array<Bssoc>
  typealias Bssoc = RefinedProtocolAdopter3
}

@inline(never)
func callThroughToOverload<T: RefinedProtocol>(arg: T) {
  arg.callOverload()
}

// CHECK: I survived in RefinedProtocolAdopter1.overload() (1)
// CHECK: Assoc := RefinedProtocolAdopter2, Bssoc := RefinedProtocolAdopter2
callThroughToOverload(arg: RefinedProtocolAdopter1())
// CHECK: I survived in RefinedProtocolAdopter2.overload() (2)
// CHECK: Assoc := RefinedProtocolAdopter2, Bssoc := RefinedProtocolAdopter1
callThroughToOverload(arg: RefinedProtocolAdopter2())
// CHECK: I survived in RefinedProtocolAdopter3.overload() (3)
// CHECK: Assoc := Array<RefinedProtocolAdopter3>, Bssoc := RefinedProtocolAdopter3
callThroughToOverload(arg: RefinedProtocolAdopter3())

