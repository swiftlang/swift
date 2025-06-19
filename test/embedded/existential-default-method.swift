// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -O) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -Osize) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

// Simple case

public protocol ProtocolWithDefaultMethod: AnyObject {
  func getInt() -> Int
}

extension ProtocolWithDefaultMethod {
  public func getInt() -> Int {
    return 42
  }
}

public class Class: ProtocolWithDefaultMethod {
}

public class GenClass<T>: ProtocolWithDefaultMethod {
}

func test(existential: any ProtocolWithDefaultMethod) {
  print(existential.getInt())
}

// Test that we specialize for the correct derived class

class C {
  class func g() -> Int {
    return 1
  }
}

class D: C {
  override class func g() -> Int {
    return 2
  }
}

protocol P: AnyObject {
  static func g() -> Int
  func test() -> Int
}

extension P {
  func test() -> Int {
    Self.g()
  }
}

extension C: P {}

func createDerived() -> P {
  return D()
}

// Test that we don't end up in an infinite recursion loop

public protocol RecursiveProto: AnyObject {
  associatedtype T: RecursiveProto
  func getInt() -> Int
  func getT() -> T
}

extension RecursiveProto {
  public func getInt() -> Int {
    return 27
  }
}

public class RecursiveClass: RecursiveProto {
  public typealias T = RecursiveClass
  public func getT() -> RecursiveClass {
    return self
  }
}

func testRecursive(existential: any RecursiveProto) {
  print(existential.getT().getInt())
}


@main
struct Main {
  static func main() {
    // CHECK: 42
    test(existential: Class())
    // CHECK: 42
    test(existential: GenClass<Int>())
    // CHECK: 2
    print(createDerived().test())
    // CHECK: 27
    testRecursive(existential: RecursiveClass())
  }
}

