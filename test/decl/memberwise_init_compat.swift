// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -print-ast-typechecked -print-access -source-filename=%s | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-COMPAT -implicit-check-not='init('
// RUN: %target-swift-ide-test -print-ast-typechecked -print-access -enable-experimental-feature ExcludePrivateFromMemberwiseInit -source-filename=%s | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-COMPAT -check-prefix=CHECK-NEW -implicit-check-not='init('
// RUN: %target-swift-ide-test -swift-version 7 -print-ast-typechecked -print-access -enable-experimental-feature ExcludePrivateFromMemberwiseInit -source-filename=%s | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-NEW -implicit-check-not='init('
// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature ExcludePrivateFromMemberwiseInit -primary-file %s > /dev/null

// REQUIRES: swift_feature_ExcludePrivateFromMemberwiseInit
// REQUIRES: swift7

// This file uses alphabetic prefixes on its declarations because swift-ide-test
// sorts decls in a module before printing them.

// - CHECK matches before, during, and after adoption of the feature
// - CHECK-COMPAT matches before and during the feature
// - CHECK-NEW matches during and after the feature

// CHECK-LABEL: struct A {
struct A {
  private var x: Int
  // CHECK: private init(x: Int)

  func forceEmission() { _ = Self.init(x:) }
}

// CHECK-LABEL: struct B {
struct B {
  private var x: Int?
  // CHECK: internal init()
  // CHECK-COMPAT: private init(x: Int? = nil)

  func forceEmission() { _ = Self.init(x:) }
}

// CHECK-LABEL: struct C {
struct C {
  private var x = 0
  // CHECK: internal init()
  // CHECK-COMPAT: private init(x: Int = 0)

  func forceEmission() { _ = Self.init(x:) }
}

// CHECK-LABEL: struct D {
struct D {
  private var x = 0
  var y: String
  // CHECK-COMPAT: private init(x: Int = 0, y: String)
  // CHECK-NEW: internal init(y: String)

  func forceEmission() { _ = Self.init(x:y:) }
}

// CHECK-LABEL: struct E {
struct E {
  var x: String
  private var y = 0
  // CHECK-NEW: internal init(x: String)
  // CHECK-COMPAT: private init(x: String, y: Int = 0)

  func forceEmission() { _ = Self.init(x:y:) }
}

// CHECK-LABEL: struct F {
struct F {
  var x: String
  private let y = 0
  // CHECK: internal init(x: String)
}

// CHECK-LABEL: struct G {
struct G {
  var x: Int

  private var y: Int {
    @storageRestrictions(initializes: x)
    init { x = newValue }
    get { x }
  }
  // CHECK: private init(y: Int)

  func forceEmission() { _ = Self.init(y:) }
}

// CHECK-LABEL: struct H {
struct H {
  var x: Int?

  private var y: Int? {
    @storageRestrictions(initializes: x)
    init { x = newValue }
    get { x }
  }
  // CHECK: internal init()
  // CHECK-COMPAT: private init(y: Int? = nil)

  func forceEmission() { _ = Self.init(y:) }
}

// CHECK-LABEL: struct I {
struct I {
  private var x: Int = 0 {
    @storageRestrictions(initializes: y)
    init { y = newValue }
    get { y }
  }

  var y: Int
  // CHECK: internal init()
  // CHECK-COMPAT: private init(x: Int = 0)

  func forceEmission() { _ = Self.init(x:) }
}

// CHECK-LABEL: struct J {
struct J {
  private var x: Int = 0 {
    @storageRestrictions(initializes: y)
    init { y = newValue }
    get { y }
  }

  var y: Int
  var z: String
  // CHECK-COMPAT: private init(x: Int = 0, z: String)
  // CHECK-NEW: internal init(z: String)

  func forceEmission() { _ = Self.init(x:z:) }
}

// CHECK-LABEL: struct K {
struct K {
  private var x: Int
  var y: Int {
    @storageRestrictions(initializes: x)
    init { x = newValue }
    get { x }
  }
  var z: String
  // CHECK: internal init(y: Int, z: String)
}

// CHECK-LABEL: struct L {
struct L {
  private var x: Int
  var y: Int = 0 {
    @storageRestrictions(initializes: x)
    init { x = newValue }
    get { x }
  }
  var z: String
  // CHECK: internal init(y: Int = 0, z: String)
}

// CHECK-LABEL: struct M {
struct M {
  private var x: Int = 0
  var y: Int = 0 {
    @storageRestrictions(initializes: x)
    init { x = newValue }
    get { x }
  }
  var z: String
  // CHECK: internal init(y: Int = 0, z: String)
}

// CHECK-LABEL: struct O1 {
struct O1 {
  fileprivate var x = 0
  var y: String
  // CHECK-COMPAT: fileprivate init(x: Int = 0, y: String)
  // CHECK-NEW: internal init(y: String)

  func forceEmission() { _ = Self.init(x:y:) }
}

// CHECK-LABEL: struct O2 {
fileprivate struct O2 {
  var x = 0
  var y: String
  // CHECK: init(x: Int = 0, y: String)
}

// CHECK-LABEL: struct P {
struct P {
  private var x: Int = 0, y = ""
  // CHECK: internal init()
  // CHECK-COMPAT: private init(x: Int = 0, y: String = "")

  func forceEmission() { _ = Self.init(x:y:) }
}

// CHECK-LABEL: struct Q {
struct Q {
  private var x: Int = 0, y = ""
  var z: String
  // CHECK-COMPAT: private init(x: Int = 0, y: String = "", z: String)
  // CHECK-NEW: internal init(z: String)

  func forceEmission() { _ = Self.init(x:y:z:) }
}

// CHECK-LABEL: struct R {
struct R {
  private lazy var x = 0
  var y: String
  // CHECK-COMPAT: private init(x: Int? = nil, y: String)
  // CHECK-NEW: internal init(y: String)

  func forceEmission() { _ = Self.init(x:y:) }
}
