// REQUIRES: concurrency

// RUN: %empty-directory(%t)

// RUN: %sourcekitd-test -req=interface-gen %s -- %s -target %target-triple -module-name MyModule | %FileCheck %s --check-prefix=SWIFTSOURCE

// RUN: %target-swift-frontend -emit-module -module-name MyModule -o %t/MyModule.swiftmodule %s
// RUN: %sourcekitd-test -req=interface-gen -module MyModule -- -target %target-triple -I %t | %FileCheck %s --check-prefix=SWIFTMODULE
// RUN: %sourcekitd-test -req=doc-info -module MyModule -- -target %target-triple -I %t | %FileCheck %s --check-prefix=DOCINFO

@propertyWrapper
public struct OrLess {
  private var threshold: Int
  private var number: Int = 0
  init(wrappedValue: Int, threshold: Int) {
    self.threshold = threshold
    self.wrappedValue = wrappedValue
  }
  public var wrappedValue: Int {
    get { return number }
    set { number = min(newValue, 12) }
  }
}

@resultBuilder
public struct IntBuilder {
    public static func buildBlock(_ val: Int) -> Int { val }
}

public struct TestStruct {

  @MainActor public func mainActorMethod() {}
  @MainActor(unsafe) public func mainActorUnsafeMethod() {}

  @OrLess(threshold: 12) public var twelveOrLess = 13

  @IntBuilder public var intValue: Int { 1 }
}

// SWIFTSOURCE: public struct TestStruct {
// SWIFTSOURCE:     @MainActor public func mainActorMethod()
// SWIFTSOURCE:     @MainActor public func mainActorUnsafeMethod()
// SWIFTSOURCE:     @MyModule.OrLess public var twelveOrLess: Int { get set }
// SWIFTSOURCE:     @MyModule.IntBuilder public var intValue: Int { get }
// SWIFTSOURCE: }

// SWIFTMODULE: public struct TestStruct {
// SWIFTMODULE:     @MainActor public func mainActorMethod()
// SWIFTMODULE:     @MainActor public func mainActorUnsafeMethod()
// SWIFTMODULE:     @MyModule.OrLess public var twelveOrLess: Int
// SWIFTMODULE:     @MyModule.IntBuilder public var intValue: Int { get }
// SWIFTMODULE: }

// DOCINFO: struct TestStruct {
// DOCINFO:     @MainActor func mainActorMethod()
// DOCINFO:     @MainActor func mainActorUnsafeMethod()
// DOCINFO:     @MyModule.OrLess var twelveOrLess: Int
// DOCINFO:     @MyModule.IntBuilder var intValue: Int { get }
// DOCINFO: }
