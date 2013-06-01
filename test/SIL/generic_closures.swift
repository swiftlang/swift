// RUN: %swift -emit-sil %s | FileCheck %s

// CHECK: sil @_T16generic_closures28generic_nondependent_contextU__FT1xQ_1ySi_Si
func generic_nondependent_context<T>(x:T, y:Int) -> Int {
  func foo() -> Int { return y }
  // CHECK: [[FOO:%.*]] = function_ref $[thin] <T> ((), (Builtin.ObjectPointer, [byref] Int64)) -> Int64, @{{closure[0-9]*}}
  // CHECK: [[FOO_SPEC:%.*]] = specialize [[FOO]], $[thin] ((), (Builtin.ObjectPointer, [byref] Int64)) -> Int64, T = T
  // CHECK: [[FOO_CLOSURE:%.*]] = partial_apply [[FOO_SPEC]]
  return foo()
}

// CHECK: sil @_T16generic_closures25generic_dependent_contextU__FT1xQ_1ySi_Q_
func generic_dependent_context<T>(x:T, y:Int) -> T {
  func foo() -> T { return x }
  // CHECK: [[FOO:%.*]] = function_ref $[thin] <T> ((), (Builtin.ObjectPointer, [byref] T)) -> T, @{{closure[0-9]*}}
  // CHECK: [[FOO_SPEC:%.*]] = specialize [[FOO]], $[thin] ((), (Builtin.ObjectPointer, [byref] T)) -> T, T = T
  // CHECK: [[FOO_CLOSURE:%.*]] = partial_apply [[FOO_SPEC]]
  return foo()
}

class NestedGeneric<U> {
  static func generic_nondependent_context<T>(x:T, y:Int, z:U) -> Int {
    func foo() -> Int { return y }
    return foo()
  }

  static func generic_dependent_inner_context<T>(x:T, y:Int, z:U) -> T {
    func foo() -> T { return x }
    return foo()
  }

  static func generic_dependent_outer_context<T>(x:T, y:Int, z:U) -> U {
    func foo() -> U { return z }
    return foo()
  }

  static func generic_dependent_both_contexts<T>(x:T, y:Int, z:U) -> (T, U) {
    func foo() -> (T, U) { return (x, z) }
    return foo()
  }
}

// CHECK: sil @_T16generic_closures24generic_curried_functionU___fT1xQ__FT1yQ0__T_ : $[thin] <T, U> ((y : U), (x : T)) -> () {
func generic_curried_function<T, U>(x:T)(y:U) { }

// FIXME: Curried entry point thunks
