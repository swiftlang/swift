// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t.swiftinterface %s
// RUN: %FileCheck %s < %t.swiftinterface --check-prefix CHECK --check-prefix COMMON

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t-resilient.swiftinterface -enable-resilience %s
// RUN: %FileCheck %s < %t-resilient.swiftinterface --check-prefix RESILIENT --check-prefix COMMON

// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule %t.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -module-name Test -emit-parseable-module-interface-path - | %FileCheck %s --check-prefix CHECK --check-prefix COMMON

// RUN: %target-swift-frontend -emit-module -o %t/TestResilient.swiftmodule -enable-resilience %t-resilient.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/TestResilient.swiftmodule -module-name TestResilient -enable-resilience -emit-parseable-module-interface-path - | %FileCheck %s --check-prefix RESILIENT --check-prefix COMMON

// COMMON: public struct HasStoredProperties {
public struct HasStoredProperties {
  // COMMON: public var computedGetter: [[INT:.*Int]] {
  // COMMON-NEXT: get
  // COMMON-NEXT: }
  public var computedGetter: Int { return 3 }

  // COMMON: public var computedGetSet: [[INT]] {
  // COMMON-NEXT: get
  // COMMON-NEXT: set
  // COMMON-NEXT: }
  public var computedGetSet: Int {
    get { return 3 }
    set {}
  }

  // COMMON: public let simpleStoredImmutable: [[INT]]{{$}}
  public let simpleStoredImmutable: Int

  // COMMON: public var simpleStoredMutable: [[INT]]{{$}}
  public var simpleStoredMutable: Int

  // CHECK: @_hasStorage public var storedWithObservers: [[BOOL:.*Bool]] {
  // RESILIENT: {{^}}  public var storedWithObservers: [[BOOL:.*Bool]] {
  // COMMON-NEXT: get
  // COMMON-NEXT: set
  // COMMON-NEXT: }
  public var storedWithObservers: Bool {
    willSet {}
  }

  // CHECK: @_hasStorage public var storedPrivateSet: [[INT]] {
  // RESILIENT: {{^}}  public var storedPrivateSet: [[INT]] {
  // COMMON-NEXT: get
  // COMMON-NEXT: }
  public private(set) var storedPrivateSet: Int

  // CHECK: private var _: [[BOOL]]
  private var privateVar: Bool

  // COMMON: public init(){{$}}
  public init() {
    self.simpleStoredImmutable = 0
    self.simpleStoredMutable = 0
    self.storedPrivateSet = 0
    self.storedWithObservers = false
    self.privateVar = false
  }

// COMMON: }
}

// COMMON: @_fixed_layout public struct BagOfVariables {
@_fixed_layout
public struct BagOfVariables {
  // COMMON: public let a: [[INT]] = 0
  public let a: Int = 0

  // COMMON: public var b: [[BOOL]] = false
  public var b: Bool = false

  // COMMON: public var c: [[INT]] = 0
  public var c: Int = 0

  // COMMON: public init()
  public init() {}

// COMMON: }
}

// COMMON: @_fixed_layout public struct HasStoredPropertiesFixedLayout {
@_fixed_layout
public struct HasStoredPropertiesFixedLayout {
  // COMMON: public var simpleStoredMutable: [[BAGOFVARIABLES:.*BagOfVariables]]
  public var simpleStoredMutable: BagOfVariables

  // COMMON: @_hasStorage public var storedWithObservers: [[BAGOFVARIABLES]] {
  // COMMON-NEXT: get
  // COMMON-NEXT: set
  // COMMON-NEXT: }
  public var storedWithObservers: BagOfVariables {
    didSet {}
  }

  // COMMON: public init(){{$}}
  public init() {
    self.simpleStoredMutable = BagOfVariables()
    self.storedWithObservers = BagOfVariables()
  }
}
