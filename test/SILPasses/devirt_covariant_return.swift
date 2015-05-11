// RUN: %target-swift-frontend -O -primary-file %s -emit-sil -sil-inline-threshold 1000 -sil-verify-all | FileCheck %s

// Make sure that we can dig all the way through the class hierarchy and
// protocol conformances with covariant return types correctly. The verifier
// should trip if we do not handle things correctly.

// CHECK-LABEL: sil hidden @_TF23devirt_covariant_return6driverFT_T_ : $@convention(thin) () -> () {
// CHECK: bb0
// CHECK: alloc_ref
// CHECK: alloc_ref
// CHECK: alloc_ref
// CHECK: function_ref @unknown1a : $@convention(thin) () -> ()
// CHECK: apply
// CHECK: function_ref @defrenestrate : $@convention(thin) () -> ()
// CHECK: apply
// CHECK: function_ref @unknown2a : $@convention(thin) () -> ()
// CHECK: apply
// CHECK: apply
// CHECK: function_ref @unknown3a : $@convention(thin) () -> ()
// CHECK: apply
// CHECK: apply
// CHECK: strong_release
// CHECK: strong_release
// CHECK: strong_release
// CHECK: tuple
// CHECK: return

@asmname("unknown1a")
func unknown1a() -> ()
@asmname("unknown1b")
func unknown1b() -> ()
@asmname("unknown2a")
func unknown2a() -> ()
@asmname("unknown2b")
func unknown2b() -> ()
@asmname("unknown3a")
func unknown3a() -> ()
@asmname("unknown3b")
func unknown3b() -> ()
@asmname("defrenestrate")
func defrenestrate() -> ()

class B<T> {
  // We do not specialize typealias's correctly now.
  //typealias X = B
  func doSomething() -> B<T> {
    unknown1a()
    return self
  }

  // See comment in protocol P
  //class func doSomethingMeta() {
  //  unknown1b()
  //}

  func doSomethingElse() {
    defrenestrate()
  }
}

class B2<T> : B<T> {
  // When we have covariance in protocols, change this to B2.
  // We do not specialize typealias correctly now.
  //typealias X = B
  override func doSomething() -> B2<T> {
    unknown2a()
    return self
  }

  // See comment in protocol P
  //override class func doSomethingMeta() {
  //  unknown2b()
  //}
}

class B3<T> : B2<T> {
  override func doSomething() -> B3<T> {
    unknown3a()
    return self
  }
}

func WhatShouldIDo<T>(b : B<T>) -> B<T> {
  b.doSomething().doSomethingElse()
  return b
}

func doSomethingWithB<T>(b : B<T>) {
  
}

struct S {}

func driver() -> () {
  var b = B<S>()
  var b2 = B2<S>()
  var b3 = B3<S>()

  WhatShouldIDo(b)
  WhatShouldIDo(b2)
  WhatShouldIDo(b3)
}

driver()


final class Payload {
  let value: Int32
  init(_ n: Int32) {
    value = n
  }

  func getValue() -> Int32 {
    return value
  }
}

class C {
   func doSomething() -> Payload? {
      return Payload(1)
   }
}


final class C1:C {
   // Override base method, but return a non-optional result
   override func doSomething() -> Payload {
      return Payload(2)
   }
}

// Check that the Optional return value from doSomething
// gets properly unwrapped into a Payload object and then further
// devirtualized.
// CHECK-LABEL: sil hidden @_TF23devirt_covariant_return7driver1FCS_2C1VSs5Int32 :
// CHECK: integer_literal $Builtin.Int32, 2
// CHECK: struct $Int32 (%{{.*}} : $Builtin.Int32)
// CHECK-NOT: class_method
// CHECK-NOT: function_ref
// CHECK: return
func driver1(var c: C1) -> Int32 {
  return c.doSomething().getValue()
}

public class Bear {
  public init?(fail: Bool) {
    if fail { return nil }
  }

  // Check that devirtualizer can handle convenience initializers, which have covariant optional
  // return types.
  // CHECK-LABEL: sil @_TFC23devirt_covariant_return4BearcfMS0_FT15delegateFailureSb9failAfterSb_GSqS0__
  // CHECK: checked_cast_br [exact] %{{.*}} : $Bear to $PolarBear
  // CHECK: upcast %{{.*}} : $Optional<PolarBear> to $Optional<Bear>
  // CHECK: }
  public convenience init?(delegateFailure: Bool, failAfter: Bool) {
    self.init(fail: delegateFailure)
    if failAfter { return nil }
  }
}

final class PolarBear: Bear {

  override public init?(fail: Bool) {
    super.init(fail: fail)
  }

  public init?(chainFailure: Bool, failAfter: Bool) {
    super.init(fail: chainFailure)
    if failAfter { return nil }
  }
}

