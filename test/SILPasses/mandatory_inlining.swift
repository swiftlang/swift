// RUN: %target-swift-frontend -primary-file %s -emit-sil -o - -verify | FileCheck %s

// These tests are deliberately shallow, because I do not want to depend on the
// specifics of SIL generation, which might change for reasons unrelated to this
// pass

func foo(x: Float) -> Float {
  return bar(x);
}

// CHECK-LABEL: sil hidden @_TF18mandatory_inlining3foo
// CHECK-NEXT: bb0(%0 : $Float):
// CHECK-NEXT: debug_value %0 : $Float  // let x
// CHECK-NEXT: return %0

@transparent func bar(x: Float) -> Float {
  return baz(x)
}

// CHECK-LABEL: sil hidden [transparent] @_TF18mandatory_inlining3bar
  // CHECK-NOT: function_ref
  // CHECK-NOT: apply
  // CHECK: return

@transparent func baz(x: Float) -> Float {
  return x;
}

// CHECK-LABEL: sil hidden [transparent] @_TF18mandatory_inlining3baz
// CHECK: return

func spam(x: Int) -> Int {
  return x
}

// CHECK-LABEL: sil hidden @_TF18mandatory_inlining4spam

@transparent func ham(x: Int) -> Int {
  return spam(x)
}

// CHECK-LABEL: sil hidden [transparent] @_TF18mandatory_inlining3ham
  // CHECK: function_ref @_TF18mandatory_inlining4spam
  // CHECK: apply
  // CHECK: return

func eggs(x: Int) -> Int {
  return ham(x)
}

// CHECK-LABEL: sil hidden @_TF18mandatory_inlining4eggs
  // CHECK: function_ref @_TF18mandatory_inlining4spam
  // CHECK: apply
  // CHECK: return

@transparent func call_auto_closure(@autoclosure x: () -> Bool) -> Bool {
  return x()
}

func test_auto_closure_with_capture(x: Bool) -> Bool {
  return call_auto_closure(x)
}

// This should be fully inlined and simply return x; however, there's a lot of
// non-SSA cruft that I don't want this test to depend on, so I'm just going
// to verify that it doesn't have any function applications left

// CHECK-LABEL: sil hidden @{{.*}}test_auto_closure_with_capture
  // CHECK-NOT: = apply
  // CHECK: return

func test_auto_closure_without_capture() -> Bool {
  return call_auto_closure(false)
}

// This should be fully inlined and simply return false, which is easier to check for

// CHECK-LABEL: sil hidden @_TF18mandatory_inlining33test_auto_closure_without_captureFT_Sb
  // CHECK: [[FV:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK: [[FALSE:%.*]] = struct $Bool ([[FV:%.*]] : $Builtin.Int1)
  // CHECK: return [[FALSE]]

@transparent func test_curried(x: Int)(y: Int) -> Int {
  return y
}

func call_uncurried(x: Int, y: Int) -> Int {
  return test_curried(x)(y: y)
}

// CHECK-LABEL: sil hidden @_TF18mandatory_inlining14call_uncurried
  // CHECK-NOT: = apply
  // CHECK: return

func call_curried(x: Int, y: Int) -> Int {
  var z = test_curried(x)
  return z(y: y)
}

// CHECK-LABEL: sil hidden @_TF18mandatory_inlining12call_curried
  // CHECK: = apply
  // CHECK: = apply
  // CHECK: return

infix operator &&& {
  associativity left
  precedence 120
}

infix operator ||| {
  associativity left
  precedence 110
}

@transparent func &&& (lhs: Bool, @autoclosure rhs: ()->Bool) -> Bool {
  if lhs {
    return rhs()
  }

  return false
}

@transparent func ||| (lhs: Bool, @autoclosure rhs: ()->Bool) -> Bool {
  if lhs {
    return true
  }

  return rhs()
}

func test_chained_short_circuit(x: Bool, y: Bool, z: Bool) -> Bool {
  return x &&& (y ||| z)
}

// The test below just makes sure there are no uninlined [transparent] calls
// left (i.e. the autoclosure and the short-circuiting boolean operators are
// recursively inlined properly)

// CHECK-LABEL: sil hidden @_TF18mandatory_inlining26test_chained_short_circuit
  // CHECK-NOT = apply [transparent]
  // CHECK: return


// Union element constructors should be inlined automatically.
enum X {
  case onetransp
  case twotransp
}

func testInlineUnionElement() -> X {
  return X.onetransp;
  // CHECK-LABEL: sil hidden @_TF18mandatory_inlining22testInlineUnionElementFT_OS_1X
  // CHECK: enum $X, #X.onetransp!enumelt
  // CHECK-NOT = apply
  // CHECK: return
}



@transparent
func call_let_auto_closure(@autoclosure let x: () -> Bool) -> Bool {
  return x()
}

// CHECK: sil hidden @{{.*}}test_let_auto_closure_with_value_capture
// CHECK-NEXT: bb0(%0 : $Bool):
// CHECK-NEXT: debug_value %0 : $Bool
// CHECK-NEXT: return %0 : $Bool

func test_let_auto_closure_with_value_capture(let x: Bool) -> Bool {
  return call_let_auto_closure(x)
}


class C {}

// CHECK-LABEL: sil hidden [transparent] @_TF18mandatory_inlining25class_constrained_genericuRdq_CS_1C_Fq_GSqPMPSs9AnyObject__ : $@convention(thin) <T where T : C> (@owned T) -> Optional<AnyObject.Type>
@transparent
func class_constrained_generic<T : C>(o: T) -> AnyClass? {
  // CHECK: return
  return T.self
}

// CHECK-LABEL: sil hidden @_TF18mandatory_inlining6invokeFCS_1CT_ : $@convention(thin) (@owned C) -> () {
func invoke(c: C) {
  // CHECK-NOT: function_ref @_TF18mandatory_inlining25class_constrained_genericuRdq_CS_1C_Fq_GSqPMPSs9AnyObject__
  // CHECK-NOT: apply
  // CHECK: init_existential_metatype
  class_constrained_generic(c)
  // CHECK: return
}
