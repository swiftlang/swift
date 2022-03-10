// REQUIRES: concurrency

// RUN: %empty-directory(%t)

// RUN: %sourcekitd-test -req=interface-gen %s -- %s -target %target-triple -module-name MyModule | %FileCheck %s --check-prefix=SWIFTSOURCE

// RUN: %target-swift-frontend -emit-module -module-name MyModule -o %t/MyModule.swiftmodule %s
// RUN: %sourcekitd-test -req=interface-gen -module MyModule -- -target %target-triple -I %t | %FileCheck %s --check-prefix=SWIFTMODULE
// RUN: %sourcekitd-test -req=doc-info -module MyModule -- -target %target-triple -I %t | %FileCheck %s --check-prefix=DOCINFO

@available(SwiftStdlib 5.1, *)
@globalActor
public struct PublicActor {
  public actor MyActor { }
  public static let shared = MyActor()
}

@available(SwiftStdlib 5.1, *)
@globalActor
struct InternalActor {
  actor MyActor { }
  static let shared = MyActor()
}

@propertyWrapper
public struct PublicOrLess {
  private var threshold: Int
  private var number: Int = 0

  public init(wrappedValue: Int, threshold: Int) {
    self.threshold = threshold
    self.wrappedValue = wrappedValue
  }
  public var wrappedValue: Int {
    get { return number }
    set { number = min(newValue, 12) }
  }
}

@propertyWrapper
struct InternalOrLess {
  private var threshold: Int
  private var number: Int = 0

  init(wrappedValue: Int, threshold: Int) {
    self.threshold = threshold
    self.wrappedValue = wrappedValue
  }
  var wrappedValue: Int {
    get { return number }
    set { number = min(newValue, 12) }
  }
}

@resultBuilder
public struct PublicBuilder {
    public static func buildBlock(_ val: Int) -> Int { val }
}

@resultBuilder
struct InternalBuilder {
  static func buildBlock(_ val: Int) -> Int { val }
}


public struct TestStruct {

  @MainActor public func mainActorMethod() {}
  @MainActor(unsafe) public func mainActorUnsafeMethod() {}

  @available(SwiftStdlib 5.1, *)
  @PublicActor public func publicActorMethod() {}
  @available(SwiftStdlib 5.1, *)
  @InternalActor public func internalActorMethod() {}

  @PublicOrLess(threshold: 12) public var publicOrLess = 13
  @InternalOrLess(threshold: 42) public var internalOrLess = 56

  @PublicBuilder public var publicBuilderVal: Int { 1 }
  @InternalBuilder public var internalBuilderVal: Int { 1 }
}

// SWIFTSOURCE: public struct TestStruct {
// SWIFTSOURCE:     @MainActor public func mainActorMethod()
// SWIFTSOURCE:     @MainActor public func mainActorUnsafeMethod()
// SWIFTSOURCE:     @MyModule.PublicActor public func publicActorMethod()
// SWIFTSOURCE:     @MyModule.InternalActor public func internalActorMethod()
// SWIFTSOURCE:     @MyModule.PublicOrLess public var publicOrLess: Int { get set }
// SWIFTSOURCE:     @MyModule.InternalOrLess public var internalOrLess: Int { get set }
// SWIFTSOURCE:     @MyModule.PublicBuilder public var publicBuilderVal: Int { get }
// SWIFTSOURCE:     @MyModule.InternalBuilder public var internalBuilderVal: Int { get }
// SWIFTSOURCE: }

// SWIFTMODULE: public struct TestStruct {
// SWIFTMODULE:     @MainActor public func mainActorMethod()
// SWIFTMODULE:     @MainActor public func mainActorUnsafeMethod()
// SWIFTMODULE:     @MyModule.PublicActor public func publicActorMethod()
// SWIFTMODULE:     public func internalActorMethod()
// SWIFTMODULE:     @MyModule.PublicOrLess public var publicOrLess: Int
// SWIFTMODULE:     public var internalOrLess: Int
// SWIFTMODULE:     @MyModule.PublicBuilder public var publicBuilderVal: Int { get }
// SWIFTMODULE:     public var internalBuilderVal: Int { get }
// SWIFTMODULE: }

// DOCINFO: struct TestStruct {
// DOCINFO:     @MainActor func mainActorMethod()
// DOCINFO:     @MainActor func mainActorUnsafeMethod()
// DOCINFO:     @MyModule.PublicActor func publicActorMethod()
// DOCINFO:     func internalActorMethod()
// DOCINFO:     @MyModule.PublicOrLess var publicOrLess: Int
// DOCINFO:     var internalOrLess: Int
// DOCINFO:     @MyModule.PublicBuilder var publicBuilderVal: Int { get }
// DOCINFO:     var internalBuilderVal: Int { get }
// DOCINFO: }
