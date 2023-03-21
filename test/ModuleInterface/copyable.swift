// RUN: %empty-directory(%t)

// This test checks that conformances to Copyable do not appear in swiftinterface files

// Generate the parseable interface of the current file via the merge-modules step
// RUN: %target-build-swift -emit-module -o %t/Test.swiftmodule -emit-module-interface-path %t/TestMerge.swiftinterface -module-name Test %s -enable-library-evolution -swift-version 5

// Generate the parseable interface of the current file via a single frontend invocation
// RUN: %target-swift-frontend -enable-library-evolution -typecheck -emit-module-interface-path %t/TestSingle.swiftinterface -module-name Test %s -enable-library-evolution -swift-version 5

// Make sure Copyable doesn't appear anywhere in these files!
// RUN: %FileCheck --implicit-check-not Copyable %s < %t/TestSingle.swiftinterface
// RUN: %FileCheck --implicit-check-not Copyable %s < %t/TestMerge.swiftinterface


// CHECK: forceGenericSubst
public func forceGenericSubst<T>(_ t: T) {
  print(t)
}

public protocol ProtocolWithAssocType {
  associatedtype SomeType
  func get() -> SomeType
}

public class BestClass: ProtocolWithAssocType {
  public typealias SomeType = BestStruct
  public func get() -> SomeType { return BestStruct() }
}

public struct BestStruct { let c = BestClass() }

public enum BestEnum<T> {
  case nothing
  case something(T)
}

public func caller(_ c: BestClass, _ s: BestStruct, _ e: BestEnum<BestStruct>) {
  forceGenericSubst(c)
  forceGenericSubst(s)
  forceGenericSubst(e)
}

public typealias TheTop = (Int, String)

public struct S<T> {
  let t: T
  init(_ t: T) { self.t = t }
}

public typealias Handler = () -> ()

public func genericFn<T>(_ t: T) -> S<T> {
  return S(t)
}

public func maker(_ top: TheTop, withCompletion comp: @escaping Handler) -> S<TheTop> {
  _ = genericFn(top)
  _ = genericFn(comp)
  return S(top)
}
