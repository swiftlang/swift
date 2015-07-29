// RUN: %target-swift-frontend -O -primary-file %s -emit-sil -sil-inline-threshold 1000 -sil-verify-all | FileCheck %s

private class Base {
  @inline(never)
  func foo() throws -> Int32? {
    print("Base")
    return 0
  }

  @inline(never)
  func boo1() throws -> Base {
    return self
  }

  @inline(never)
  func boo2() throws -> Base? {
    return self
  }
}


private class Derived1: Base {
  @inline(never)
  override func foo() throws -> Int32? {
    print("Derived1")
    return 1
  }

  @inline(never)
  override func boo1() throws -> Derived1 {
    return self
  }

  @inline(never)
  override func boo2() throws -> Derived1? {
    return self
  }
}

private class Derived2: Base {
  @inline(never)
  override func foo() throws -> Int32 {
    print("Derived2")
    return 2
  }

  @inline(never)
  override func boo1() throws -> Derived2 {
    return self
  }

  @inline(never)
  override func boo2() throws -> Derived2 {
    return self
  }
}

protocol P {
  func foo() throws -> Int32
}


class CP1: P {
  func foo() throws -> Int32 {
     return 1
  }
}

class CP2: CP1 {
  override func foo() throws -> Int32 {
     return 2
  }
}

// Check devirtualization of witness_method instructions.
// CHECK-LABEL: sil @_TF16devirt_try_apply5test4FT_VSs5Int32
// CHECK-NEXT: bb0:
// CHECK-NEXT: integer_literal $Builtin.Int32, 2
// CHECK-NEXT: struct $Int32
// CHECK-NEXT: return
public func test4() -> Int32 {
  let p = CP2()
  var result: Int32 = 0
  do {
    result = try p.foo()
  } catch _ {}
  return result
}

// Check devirtualization of witness_method instructions.
// CHECK-LABEL: sil @_TF16devirt_try_apply5test5FT_VSs5Int32
// CHECK-NEXT: bb0:
// CHECK-NEXT: integer_literal $Builtin.Int32, 1
// CHECK-NEXT: struct $Int32
// CHECK-NEXT: return
public func test5() -> Int32 {
  let p: P = CP1()
  var result: Int32 = 0
  do {
    result = try p.foo()
  } catch _ {}
  return result
}


// CHECK-LABEL: sil private [noinline] @_TTSf4g___TF16devirt_try_applyP33_E45F5529CC31A51875E58096B25575A219testTryApplyDevirt1
// CHECK-NOT: class_method
// CHECK-NOT: }
// CHECK: function_ref @_TFC16devirt_try_applyP33_E45F5529CC31A51875E58096B25575A28Derived13foofS0_FzT_GSqVSs5Int32_ : $@convention(method) (@guaranteed Derived1) -> (Optional<Int32>, @error ErrorType)
// CHECK: try_apply
// CHECK-NOT: class_method
// CHECK-NOT: }
// CHECK: function_ref @_TFC16devirt_try_applyP33_E45F5529CC31A51875E58096B25575A28Derived23foofS0_FzT_VSs5Int32 : $@convention(method) (@guaranteed Derived2) -> (Int32, @error ErrorType)
// CHECK: try_apply
// CHECK-NOT: class_method
// CHECK-NOT: }
// CHECK: function_ref @_TFC16devirt_try_applyP33_E45F5529CC31A51875E58096B25575A24Base3foofS0_FzT_GSqVSs5Int32_ : $@convention(method) (@guaranteed Base) -> (Optional<Int32>, @error ErrorType)
// CHECK: try_apply 
// CHECK-NOT: class_method
// CHECK: }
@inline(never)
private func testTryApplyDevirt1(b: Base) -> Int32? {
  var result: Int32? = nil
  do {
    result = try b.foo()
  } catch _ {
  }
  return result
} 

// CHECK-LABEL: sil private [noinline] @_TTSf4g___TF16devirt_try_applyP33_E45F5529CC31A51875E58096B25575A219testTryApplyDevirt2
// CHECK-NOT: class_method
// CHECK-NOT: }
// CHECK: function_ref @_TFC16devirt_try_applyP33_E45F5529CC31A51875E58096B25575A28Derived14boo1fS0_FzT_S0_ : $@convention(method) (@guaranteed Derived1) -> (@owned Derived1, @error ErrorType)
// CHECK: try_apply
// CHECK-NOT: class_method
// CHECK-NOT: }
// CHECK: function_ref @_TFC16devirt_try_applyP33_E45F5529CC31A51875E58096B25575A28Derived24boo1fS0_FzT_S0_ : $@convention(method) (@guaranteed Derived2) -> (@owned Derived2, @error ErrorType)
// CHECK: try_apply
// CHECK-NOT: class_method
// CHECK-NOT: }
// CHECK: function_ref @_TFC16devirt_try_applyP33_E45F5529CC31A51875E58096B25575A24Base4boo1fS0_FzT_S0_ : $@convention(method) (@guaranteed Base) -> (@owned Base, @error ErrorType)
// CHECK: try_apply 
// CHECK-NOT: class_method
// CHECK: }
@inline(never)
private func testTryApplyDevirt2(b: Base) -> Base? {
  var result: Base? = nil
  do {
    result = try b.boo1()
  } catch _ {
  }
  return result
} 

// CHECK-LABEL: sil private [noinline] @_TTSf4g___TF16devirt_try_applyP33_E45F5529CC31A51875E58096B25575A219testTryApplyDevirt3
// CHECK-NOT: class_method
// CHECK-NOT: }
// CHECK: function_ref @_TFC16devirt_try_applyP33_E45F5529CC31A51875E58096B25575A28Derived14boo2fS0_FzT_GSqS0__ : $@convention(method) (@guaranteed Derived1) -> (@owned Optional<Derived1>, @error ErrorType)
// CHECK: try_apply
// CHECK-NOT: class_method
// CHECK-NOT: }
// CHECK: function_ref @_TFC16devirt_try_applyP33_E45F5529CC31A51875E58096B25575A28Derived24boo2fS0_FzT_S0_ : $@convention(method) (@guaranteed Derived2) -> (@owned Derived2, @error ErrorType)
// CHECK: try_apply
// CHECK-NOT: class_method
// CHECK-NOT: }
// CHECK: function_ref @_TFC16devirt_try_applyP33_E45F5529CC31A51875E58096B25575A24Base4boo2fS0_FzT_GSqS0__ : $@convention(method) (@guaranteed Base) -> (@owned Optional<Base>, @error ErrorType)
// CHECK: try_apply 
// CHECK-NOT: class_method
// CHECK: }
@inline(never)
private func testTryApplyDevirt3(b: Base) -> Base? {
  var result: Base? = nil
  do {
    result = try b.boo2()
  } catch _ {
  }
  return result
} 

public func test1() {
  testTryApplyDevirt1(Base())
}

public func test2() {
  testTryApplyDevirt2(Base())
}

public func test3() {
  testTryApplyDevirt3(Base())
}

