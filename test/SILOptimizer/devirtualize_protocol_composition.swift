// RUN: %target-swift-frontend -parse-as-library -O -wmo -emit-sil  %s | %FileCheck %s
// RUN: %target-swift-frontend -parse-as-library -Osize -wmo -emit-sil  %s | %FileCheck %s

// This is an end-to-end test to ensure that the optimizer devertualizes
// calls to a protocol composition type.

public class ClassA<T> { }

protocol ProtocolA {
  func foo() -> Int
}

protocol ProtocolB {
  func bar() -> Int
}

protocol ProtocolC {
  func foo() -> Int
}

protocol ProtocolD {
  func foo() -> Int
}

protocol ProtocolE {
  func foo() -> Int
}

protocol ProtocolF {
  func foo() -> Int
}

protocol ProtocolG {
  func bar() -> Int
}

protocol ProtocolH {
  func bar() -> Int
}

public class ClassB: ClassA<String> {
  func foo() -> Int {
    return 10
  }
}

extension ClassB: ProtocolA { }

public class ClassC<T>: ClassA<T> {
  func foo() -> Int {
    return 10
  }
}

extension ClassC: ProtocolC { }

public class ClassD { }
public class ClassE : ClassD {
  func foo() -> Int {
    return 10
  }
}

extension ClassE: ProtocolD { }

public class ClassF {
  func foo() -> Int {
    return 10
  }
  
  func bar() -> Int {
    return 10
  }
}

extension ClassF: ProtocolE, ProtocolG { }

public class ClassG <T> {
  func foo() -> Int {
    return 10
  }
  
  func bar() -> Int {
    return 10
  }
}

extension ClassG: ProtocolF, ProtocolH { }

public class ClassH {
  typealias type = ClassD
}

func shouldOptimize1<T>(_ x: ClassA<T> & ProtocolA) -> Int {
  return x.foo()
}

func shouldOptimize2(_ x: ClassD & ProtocolD) -> Int {
  return x.foo()
}

func shouldOptimize3(_ x: ProtocolE & ProtocolG) -> Int {
  return x.foo() + x.bar()
}

func shouldOptimize4(_ x: ProtocolF & ProtocolH) -> Int {
  return x.foo() + x.bar()
}

func shouldOptimize5(_ x: ClassH.type & ProtocolD) -> Int {
  return x.foo()
}

//CHECK: entryPoint1
//CHECK-NOT: init_existential_ref
//CHECK-NOT: open_existential_ref
//CHECK-NOT: witness_method
//CHECK: return
public func entryPoint1(c: ClassB) -> Int {
  return shouldOptimize1(c)
}

// TODO: is this broken on master too?
//public func entryPoint2<T>(c: ClassC<T>) -> Int {
//  return shouldOptimize1(c)
//}

//CHECK: entryPoint3
//CHECK-NOT: init_existential_ref
//CHECK-NOT: open_existential_ref
//CHECK-NOT: witness_method
//CHECK: return
public func entryPoint3(c: ClassE) -> Int {
  return shouldOptimize2(c)
}

//CHECK: entryPoint4
//CHECK-NOT: init_existential_ref
//CHECK-NOT: open_existential_ref
//CHECK-NOT: witness_method
//CHECK: return
public func entryPoint4(c: ClassF) -> Int {
  return shouldOptimize3(c)
}

//CHECK: entryPoint5
//CHECK-NOT: init_existential_ref
//CHECK-NOT: open_existential_ref
//CHECK-NOT: witness_method
//CHECK: return
public func entryPoint5<T>(c: ClassG<T>) -> Int {
  return shouldOptimize4(c)
}

//CHECK: entryPoint6
//CHECK-NOT: init_existential_ref
//CHECK-NOT: open_existential_ref
//CHECK-NOT: witness_method
//CHECK: return
public func entryPoint6(c: ClassE) -> Int {
  return shouldOptimize5(c)
}
