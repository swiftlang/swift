// RUN: %target-swift-frontend -parse-as-library -O -wmo -emit-sil  %s | %FileCheck %s
// RUN: %target-swift-frontend -parse-as-library -Osize -wmo -emit-sil  %s | %FileCheck %s

// This is an end-to-end test to ensure that the optimizer devertualizes
// calls to a protocol composition type.

public class ClassA<T> { }

protocol ProtocolA {
  func foo() -> Int
}

func shouldOptimizeWitness<T>(_ x: ClassA<T> & ProtocolA) -> Int {
  return x.foo()
}

public class ClassB: ClassA<String> {
  func foo() -> Int {
    return 10
  }
}
extension ClassB: ProtocolA { }

//CHECK: witnessEntryPoint
//CHECK-NOT: init_existential_ref
//CHECK-NOT: open_existential_ref
//CHECK-NOT: witness_method
//CHECK: return
public func witnessEntryPoint(c: ClassB) -> Int {
  return shouldOptimizeWitness(c)
}
