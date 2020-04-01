// Note: this test has a client: inherits-superclass-initializers-client.swift

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -emit-module-interface-path %t/Module.swiftinterface -module-name Module -enable-library-evolution
// RUN: %FileCheck %s < %t/Module.swiftinterface

// CHECK: @_hasMissingDesignatedInitializers open class Base {
open class Base {
  // CHECK-NEXT: public init(arg: Swift.Int)
  public init(arg: Int) {
    print("public init from Base")
  }
  // CHECK-NOT: init(secret: Swift.Int)
  internal init(secret: Int) {
    print("secret init from Base")
  }

  // CHECK: convenience public init()
  public convenience init() {
    self.init(secret: 4)
  }

// CHECK: }
}

// CHECK-NOT: @_hasMissingDesignatedInitializers
// CHECK: open class InlineBase {
open class InlineBase {
  // CHECK-NEXT: public init(arg: Swift.Int)
  public init(arg: Int) {
    print("public init from Inline Base")
  }

  // CHECK-NOT: @usableFromInline init(secret: Swift.Int)
  @usableFromInline
  internal init(secret: Int) {
    print("secret init from Inline Base")
  }

  // CHECK: convenience public init()
  public convenience init() {
    self.init(secret: 42)
  }
  // CHECK: }
}

// CHECK: @_inheritsConvenienceInitializers @_hasMissingDesignatedInitializers public class Sub : Module.Base {
public class Sub : Base {
  // CHECK: override public init(arg: Swift.Int)
  public override init(arg: Int) {
    print("public init from Sub")
    super.init(arg: arg)
  }
  // CHECK-NOT: init(secret: Swift.Int)
  internal override init(secret: Int) {
    print("secret init from Sub")
    super.init(secret: secret)
  }
// CHECK: }
}

// CHECK: @_inheritsConvenienceInitializers @_hasMissingDesignatedInitializers public class SubSub : Module.Sub {
public class SubSub: Sub {
  // CHECK: override public init(arg: Swift.Int)
  public override init(arg: Int) {
    print("public init from SubSub")
    super.init(arg: arg)
  }
  // CHECK-NOT: init(secret: Swift.Int)
  internal override init(secret: Int) {
    print("secret init from SubSub")
    super.init(secret: secret)
  }
// CHECK: }
}

@inlinable public func test() {
  _ = Sub()
  _ = SubSub()
}
