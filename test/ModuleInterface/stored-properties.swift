// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t.swiftinterface -module-name StoredProperties %s
// RUN: %FileCheck %s < %t.swiftinterface --check-prefix CHECK --check-prefix COMMON

// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t-resilient.swiftinterface -module-name StoredProperties -enable-library-evolution %s
// RUN: %FileCheck %s < %t-resilient.swiftinterface --check-prefix RESILIENT --check-prefix COMMON

// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule -module-name StoredProperties %t.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -module-name StoredProperties -emit-module-interface-path - | %FileCheck %s --check-prefix CHECK --check-prefix COMMON

// RUN: %target-swift-frontend -emit-module -o %t/TestResilient.swiftmodule -module-name StoredProperties -enable-library-evolution %t-resilient.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/TestResilient.swiftmodule -module-name StoredProperties -enable-library-evolution -emit-module-interface-path - | %FileCheck %s --check-prefix RESILIENT --check-prefix COMMON

// COMMON: public struct HasStoredProperties {
public struct HasStoredProperties {
  // COMMON: public var computedGetter: Swift.Int {
  // COMMON-NEXT: get
  // COMMON-NEXT: }
  public var computedGetter: Int { return 3 }

  // COMMON: public var computedGetSet: Swift.Int {
  // COMMON-NEXT: get
  // COMMON-NEXT: set
  // COMMON-NEXT: }
  public var computedGetSet: Int {
    get { return 3 }
    set {}
  }

  // COMMON: public let simpleStoredImmutable: Swift.Int{{$}}
  public let simpleStoredImmutable: Int

  // COMMON: public var simpleStoredMutable: Swift.Int{{$}}
  public var simpleStoredMutable: Int

  // CHECK: @_hasStorage public var storedWithObservers: Swift.Bool {
  // RESILIENT:   {{^}}  public var storedWithObservers: Swift.Bool {
  // COMMON-NEXT: {{^}}    get
  // COMMON-NEXT: {{^}}    set
  // COMMON-NEXT: {{^}}  }
  public var storedWithObservers: Bool {
    willSet {}
  }

  // CHECK: @_hasStorage public var storedPrivateSet: Swift.Int {
  // RESILIENT:   {{^}}  public var storedPrivateSet: Swift.Int {
  // COMMON-NEXT: {{^}}    get
  // COMMON-NEXT: {{^}}  }
  public private(set) var storedPrivateSet: Int

  // CHECK: private var privateVar: Swift.Bool
  // RESILIENT-NOT: private var privateVar: Swift.Bool
  private var privateVar: Bool

  // CHECK: @_hasStorage @_hasInitialValue public var storedWithObserversInitialValue: Swift.Int {
  // RESILIENT:   {{^}}  public var storedWithObserversInitialValue: Swift.Int {
  // COMMON-NEXT: {{^}}    get
  // COMMON-NEXT: {{^}}    set
  // COMMON-NEXT: {{^}}  }
  public var storedWithObserversInitialValue: Int = 0 {
    didSet {}
  }

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

// COMMON: @frozen public struct BagOfVariables {
@frozen
public struct BagOfVariables {
  // COMMON: private let hidden: Swift.Int = 0
  private let hidden: Int = 0

  // COMMON: public let a: Swift.Int = 0
  public let a: Int = 0

  // COMMON: public var b: Swift.Bool = false
  public var b: Bool = false

  // COMMON: public var c: Swift.Int = 0
  public var c: Int = 0

  // COMMON: public init()
  public init() {}

// COMMON: }
}

// COMMON: @frozen public struct HasStoredPropertiesFixedLayout {
@frozen
public struct HasStoredPropertiesFixedLayout {
  // COMMON: public var simpleStoredMutable: StoredProperties.BagOfVariables
  public var simpleStoredMutable: BagOfVariables

  // COMMON:      {{^}} @_hasStorage public var storedWithObservers: StoredProperties.BagOfVariables {
  // COMMON-NEXT: {{^}}    get
  // COMMON-NEXT: {{^}}    set
  // COMMON-NEXT: {{^}} }
  public var storedWithObservers: BagOfVariables {
    didSet {}
  }

  // COMMON: public init(){{$}}
  public init() {
    self.simpleStoredMutable = BagOfVariables()
    self.storedWithObservers = BagOfVariables()
  }
}
