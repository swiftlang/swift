// RUN: %swift-ide-test -print-ast-typechecked -print-implicit-attrs -source-filename %s -target x86_64-apple-macosx10.9 | FileCheck %s

// CHECK: BaseClass
class BaseClass {
  func overridden() {}
  // CHECK: {{^}} func overridden

  func nonOverridden() {}
  // CHECK: final func nonOverridden

  var nonOverriddenVar: Int
  // CHECK: final var nonOverriddenVar

  var overriddenAccessor: Int {
    get {}
    set {}
  }
  // CHECK: {{^}} var overriddenAccessor
  // CHECK-NEXT: {{^}} get
  // CHECK-NEXT: {{^}} set

  var nonOverriddenAccessor: Int {
    get {}
    set {}
  }
  // CHECK: final var nonOverriddenAccessor
  // CHECK-NEXT: final get
  // CHECK-NEXT: final set

  var overriddenObserver: Int {
    willSet{} // this actually ends up being final anyways
    didSet{}  // this actually ends up being final anyways
  }
  // CHECK: {{^}} var overriddenObserver
  // CHECK-NEXT: final willSet
  // CHECK-NEXT: final didSet

  var nonOverriddenObserver: Int {
    willSet{}
    didSet{}
  }
  // CHECK: final var nonOverriddenObserver
  // CHECK-NEXT: final willSet
  // CHECK-NEXT: final didSet

  private func privateFunc() { }
  // CHECK: private final func privateFunc

  private func overriddenPrivateFunc() { }
  // CHECK: {{^}} private func overriddenPrivateFunc

  subscript(i : Int) -> Int { return 1 }
  // CHECK: {{^}} subscript

  init() {}
  // CHECK: {{^}} init

  // CHECK: @objc deinit
}

// CHECK: Derived1
class Derived1: BaseClass {
  override func overridden() {}
  // CHECK override final func overridden

  override subscript(i : Int) -> Int { return 2 }
  // CHECK: override final subscript

  // CHECK: @objc deinit
  // CHECK: init
}

// CHECK: Derived2
class Derived2: BaseClass {
  override var overriddenAccessor: Int {
    get {}
    set {}
  }
  // CHECK: override final var overriddenAccessor
  // CHECK-NEXT: override final get
  // CHECK-NEXT: override final set

  override var overriddenObserver: Int {
    willSet{}
  }
  // CHECK: override final var overriddenObserver
  // CHECK-NEXT: final willSet

  // CHECK: @objc deinit
  // CHECK: init
}

// CHECK: Derived3
class Derived3 : BaseClass {
  override func overriddenPrivateFunc() {}
  // CHECK: {{^}} override func overriddenPrivateFunc

  // CHECK: @objc deinit
  // CHECK: {{^}} init
}

// CHECK: DerivedDerived
class DerivedDerived : Derived3 {
  override func overriddenPrivateFunc() {}
  // CHECK: override final func overriddenPrivateFunc

  // CHECK: @objc deinit
  // CHECK: init
}

