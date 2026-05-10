// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/functions
// RUN: %target-codesign %t/functions

// RUN: %target-run %target-swift-reflection-test %t/functions | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize %add_num_extra_inhabitants

// FIXME: Should not require objc_interop -- please put Objective-C-specific
// testcases in functions_objc.swift

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

/*
   This file pokes at the swift_reflection_infoForInstance() API
   of the SwiftRemoteMirror library.

   It tests introspection of function closure contexts.

   - See also: SwiftReflectionTest.reflect(function:)
*/

import SwiftReflectionTest

@_optimize(none)
func concrete(x: Int, y: Any) {
  reflect(function: {print(x)})
// CHECK:         Type reference:
// CHECK-NEXT:    (builtin Builtin.NativeObject)

// CHECK-32:      Type info:
// CHECK-32-NEXT: (closure_context size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field offset=8
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))

// CHECK-64:      Type info:
// CHECK-64-NEXT: (closure_context size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field offset=16
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))

  // Here the context is a single boxed value
  reflect(function: {print(y)})
// CHECK:         Type reference:
// CHECK-NEXT:    (builtin Builtin.NativeObject)

// CHECK-32:      Type info:
// CHECK-32-NEXT: (closure_context size=24 alignment=4 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field offset=8
// CHECK-32-NEXT:     (opaque_existential size=16 alignment=4 stride=16 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=metadata offset=12
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1)))))

// CHECK-64:      Type info:
// CHECK-64-NEXT: (closure_context size=48 alignment=8 stride=48 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field offset=16
// CHECK-64-NEXT:     (opaque_existential size=32 alignment=8 stride=32 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=metadata offset=24
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1)))))
}

concrete(x: 10, y: true)

protocol P {}
extension Int : P {}

class C {
  func captureWeakSelf() -> () -> () {
    return { [weak self] in
      print(self)
    }
  }

  func captureUnownedSelf() -> () -> () {
    return { [unowned self] in
      print(self)
    }
  }
}

@_optimize(none)
func generic<T : P, U, V : C>(x: T, y: U, z: V, i: Int) {
  reflect(function: {print(i)})
// CHECK:         Type reference:
// CHECK-NEXT:    (builtin Builtin.NativeObject)

// CHECK-32:      Type info:
// CHECK-32-NEXT: (closure_context size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field offset=8
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:       (field name=_value offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)))))

// CHECK-64:      Type info:
// CHECK-64-NEXT: (closure_context size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field offset=16
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:       (field name=_value offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))

  reflect(function: {print(x); print(y); print(z)})
// CHECK:         Type reference:
// CHECK-NEXT:    (builtin Builtin.NativeObject)

// CHECK-32:      Type info:
// CHECK-32-NEXT:(closure_context size=44 alignment=4 stride=44 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:  (field offset=24
// CHECK-32-NEXT:    (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:      (field name=_value offset=0
// CHECK-32-NEXT:        (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:  (field offset=28
// CHECK-32-NEXT:    (struct size=12 alignment=4 stride=12 num_extra_inhabitants=253 bitwise_takable=1
// CHECK-32-NEXT:      (field name=_guts offset=0
// CHECK-32-NEXT:        (struct size=12 alignment=4 stride=12 num_extra_inhabitants=253 bitwise_takable=1
// CHECK-32-NEXT:          (field name=_object offset=0
// CHECK-32-NEXT:            (struct size=12 alignment=4 stride=12 num_extra_inhabitants=253 bitwise_takable=1
// CHECK-32-NEXT:              (field name=_count offset=0
// CHECK-32-NEXT:                (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:                  (field name=_value offset=0
// CHECK-32-NEXT:                    (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:              (field name=_variant offset=4
// CHECK-32-NEXT:                (multi_payload_enum size=5 alignment=4 stride=8 num_extra_inhabitants=253 bitwise_takable=1
// CHECK-32-NEXT:                  (case name=immortal index=0 offset=0
// CHECK-32-NEXT:                    (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:                      (field name=_value offset=0
// CHECK-32-NEXT:                        (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:                  (case name=native index=1 offset=0
// CHECK-32-NEXT:                    (class_existential size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:                      (field name=object offset=0
// CHECK-32-NEXT:                        (reference kind=strong refcounting=unknown))))
// CHECK-32-NEXT:                  (case name=bridged index=2 offset=0
// CHECK-32-NEXT:                    (class_existential size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:                      (field name=object offset=0
// CHECK-32-NEXT:                        (reference kind=strong refcounting=unknown))))))
// CHECK-32-NEXT:              (field name=_discriminator offset=9
// CHECK-32-NEXT:                (struct size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:                  (field name=_value offset=0
// CHECK-32-NEXT:                    (builtin size=1 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:              (field name=_flags offset=10
// CHECK-32-NEXT:                (struct size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:                  (field name=_value offset=0
// CHECK-32-NEXT:                    (builtin size=2 alignment=2 stride=2 num_extra_inhabitants=0 bitwise_takable=1))))))))))
// CHECK-32-NEXT:  (field offset=40
// CHECK-32-NEXT:    (reference kind=strong refcounting=native)))

// CHECK-64:      Type info:
// CHECK-64-NEXT:(closure_context size=80 alignment=8 stride=80 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:  (field offset=48
// CHECK-64-NEXT:    (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:      (field name=_value offset=0
// CHECK-64-NEXT:        (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:  (field offset=56
// CHECK-64-NEXT:    (struct size=16 alignment=8 stride=16 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64-NEXT:      (field name=_guts offset=0
// CHECK-64-NEXT:        (struct size=16 alignment=8 stride=16 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64-NEXT:          (field name=_object offset=0
// CHECK-64-NEXT:            (struct size=16 alignment=8 stride=16 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
// CHECK-64-NEXT:              (field name=_countAndFlagsBits offset=0
// CHECK-64-NEXT:                (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:                  (field name=_value offset=0
// CHECK-64-NEXT:                    (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:              (field name=_object offset=8
// CHECK-64-NEXT:                (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1))))))))
// CHECK-64-NEXT:  (field offset=72
// CHECK-64-NEXT:    (reference kind=strong refcounting=native)))
}

generic(x: 10, y: "", z: C(), i: 101)

class GC<A, B, C> {}

@_optimize(none)
func genericWithSources<A, B, C>(a: A, b: B, c: C, gc: GC<A, B, C>) {
  reflect(function: {print(a); print(b); print(c); print(gc)})
// CHECK:         Type reference:
// CHECK-NEXT:    (builtin Builtin.NativeObject)

// CHECK-32:      Type info:
// CHECK-32-NEXT:(closure_context size=24 alignment=4 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:  (field offset=20
// CHECK-32-NEXT:    (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-32-NEXT:  (field offset=20
// CHECK-32-NEXT:    (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:      (field offset=0
// CHECK-32-NEXT:        (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-32-NEXT:      (field offset=0
// CHECK-32-NEXT:        (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:  (field offset=20
// CHECK-32-NEXT:    (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:      (field offset=0
// CHECK-32-NEXT:        (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-32-NEXT:      (field offset=0
// CHECK-32-NEXT:        (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-32-NEXT:      (field offset=0
// CHECK-32-NEXT:        (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-32-NEXT:  (field offset=20
// CHECK-32-NEXT:    (reference kind=strong refcounting=native)))

// CHECK-64:      Type info:
// CHECK-64-NEXT:(closure_context size=48 alignment=8 stride=48 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:  (field offset=40
// CHECK-64-NEXT:    (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-64-NEXT:  (field offset=40
// CHECK-64-NEXT:    (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:      (field offset=0
// CHECK-64-NEXT:        (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-64-NEXT:      (field offset=0
// CHECK-64-NEXT:        (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:  (field offset=40
// CHECK-64-NEXT:    (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:      (field offset=0
// CHECK-64-NEXT:        (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-64-NEXT:      (field offset=0
// CHECK-64-NEXT:        (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))
// CHECK-64-NEXT:      (field offset=0
// CHECK-64-NEXT:        (tuple size=0 alignment=1 stride=1 num_extra_inhabitants=0 bitwise_takable=1))))
// CHECK-64-NEXT:  (field offset=40
// CHECK-64-NEXT:    (reference kind=strong refcounting=native)))
}

genericWithSources(a: (), b: ((), ()), c: ((), (), ()), gc: GC<(), ((), ()), ((), (), ())>())


class CapturingClass {

  // CHECK: Reflecting an object.
  // CHECK: Type reference:
  // CHECK-NEXT: (class functions.CapturingClass)
 
  // CHECK-64: Type info:
  // CHECK-64-NEXT: (class_instance size=16 alignment=1 stride=16
  
  // CHECK-32: Type info:
  // CHECK-32-NEXT: (class_instance size=8 alignment=1 stride=8
  @_optimize(none)
  func arity0Capture1() -> () -> () {
    let closure = {
      // Captures a single retainable reference.
      print(self)
    }
    reflect(function: closure)
    return closure
  }

  // CHECK: Reflecting an object.
  // CHECK: Type reference:
  // CHECK-NEXT: (builtin Builtin.NativeObject)
  
  // CHECK-64:        Type info:
  // CHECK-64-NEXT: (closure_context size=32 alignment=8 stride=32 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:   (field offset=16
  // CHECK-64-NEXT:     (tuple size=16 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:       (field offset=0
  // CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:           (field name=_value offset=0
  // CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
  // CHECK-64-NEXT:       (field offset=8
  // CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:           (field name=_value offset=0
  // CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))

  // CHECK-32:        Type info:
  // CHECK-32-NEXT: (closure_context size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:   (field offset=8
  // CHECK-32-NEXT:     (tuple size=16 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:       (field offset=0
  // CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:           (field name=_value offset=0
  // CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
  // CHECK-32-NEXT:       (field offset=8
  // CHECK-32-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:           (field name=_value offset=0
  // CHECK-32-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))
  @_optimize(none)
  func arity1Capture1() -> (Int) -> () {
    let pair = (2, 333.0)
    let closure = { (i: Int) in
      print(pair)
    }
    reflect(function: closure)
    return closure
  }

  // CHECK: Reflecting an object.
  // CHECK: Type reference:
  // CHECK-NEXT: (builtin Builtin.NativeObject)

  // CHECK-64:      Type info:
  // CHECK-64-NEXT: (closure_context size=32 alignment=8 stride=32 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT: (field offset=16
  // CHECK-64-NEXT:   (tuple size=16 alignment=8 stride=16 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
  // CHECK-64-NEXT:     (field offset=0
  // CHECK-64-NEXT:       (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:         (field name=_value offset=0
  // CHECK-64-NEXT:           (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
  // CHECK-64-NEXT:     (field offset=8
  // CHECK-64-NEXT:       (reference kind=strong refcounting=native)))))

  // CHECK-32:        Type info:
  // CHECK-32-NEXT: (closure_context size=16 alignment=4 stride=16 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:   (field offset=8
  // CHECK-32-NEXT:     (tuple size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
  // CHECK-32-NEXT:       (field offset=0
  // CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:           (field name=_value offset=0
  // CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
  // CHECK-32-NEXT:       (field offset=4
  // CHECK-32-NEXT:         (reference kind=strong refcounting=native)))))
  @_optimize(none)
  func arity2Capture1() -> (Int, String) -> () {
   let pair = (999, C())
   let closure = { (i: Int, s: String) in
     print(pair)
   }

   reflect(function: closure)
   return closure
  }

  // CHECK: Reflecting an object.
  // CHECK: Type reference:
  // CHECK-NEXT: (builtin Builtin.NativeObject)
  
  // CHECK-64:        Type info:
  // CHECK-64-NEXT: (closure_context size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:   (field offset=16
  // CHECK-64-NEXT:     (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit-1]] bitwise_takable=1
  // CHECK-64-NEXT:       (case name=some index=0 offset=0
  // CHECK-64-NEXT:         (class_existential size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit]] bitwise_takable=1
  // CHECK-64-NEXT:           (field name=object offset=0
  // CHECK-64-NEXT:             (reference kind=strong refcounting=unknown))))
  // CHECK-64-NEXT:       (case name=none index=1))))

  // CHECK-32:        Type info:
  // CHECK-32-NEXT: (closure_context size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:   (field offset=8
  // CHECK-32-NEXT:     (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095 bitwise_takable=1
  // CHECK-32-NEXT:       (case name=some index=0 offset=0
  // CHECK-32-NEXT:         (class_existential size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1
  // CHECK-32-NEXT:           (field name=object offset=0
  // CHECK-32-NEXT:             (reference kind=strong refcounting=unknown))))
  // CHECK-32-NEXT:       (case name=none index=1))))
  @_optimize(none)
  func arity3Capture1() -> (Int, String, AnyObject?) -> () {
    let c: AnyObject? = C()
    let closure = { (i: Int, s: String, a: AnyObject?) in
      print(c)
    }
  
    reflect(function: closure)
    return closure
  }


  // CHECK: Reflecting an object.
  // CHECK: Type reference:
  // CHECK-NEXT: (builtin Builtin.NativeObject)

  // CHECK-64: Type info:
  // CHECK-64-NEXT: (closure_context size=40 alignment=8 stride=40 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:   (field offset=16
  // CHECK-64-NEXT:     (reference kind=strong refcounting=native))
  // CHECK-64-NEXT:   (field offset=24
  // CHECK-64-NEXT:     (tuple size=16 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:       (field offset=0
  // CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:           (field name=_value offset=0
  // CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
  // CHECK-64-NEXT:       (field offset=8
  // CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:           (field name=_value offset=0
  // CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))

  // CHECK-32:        Type info:
  // CHECK-32-NEXT: (closure_context size=32 alignment=8 stride=32 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:   (field offset=8
  // CHECK-32-NEXT:     (reference kind=strong refcounting=native))
  // CHECK-32-NEXT:   (field offset=16
  // CHECK-32-NEXT:     (tuple size=16 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:       (field offset=0
  // CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:           (field name=_value offset=0
  // CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
  // CHECK-32-NEXT:       (field offset=8
  // CHECK-32-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:           (field name=_value offset=0
  // CHECK-32-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))
  @_optimize(none)
  func arity0Capture2() -> () -> () {
   let pair = (999, 1010.2)
    let closure = {
      print(self)
      print(pair)
    }
    reflect(function: closure)
    return closure
  }

  // CHECK: Reflecting an object.
  // CHECK: Type reference:
  // CHECK-NEXT: (builtin Builtin.NativeObject)
  
  // CHECK-64:        Type info:
  // CHECK-64-NEXT: (closure_context size=32 alignment=8 stride=32 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:   (field offset=16
  // CHECK-64-NEXT:     (reference kind=strong refcounting=native))
  // CHECK-64-NEXT:   (field offset=24
  // CHECK-64-NEXT:     (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=[[#num_extra_inhabitants_64bit-1]] bitwise_takable=1
  // CHECK-64-NEXT:       (case name=some index=0 offset=0
  // CHECK-64-NEXT:         (reference kind=strong refcounting=native))
  // CHECK-64-NEXT:       (case name=none index=1))))

  // CHECK-32: Type info:
  // CHECK-32-NEXT: (closure_context size=16 alignment=4 stride=16 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:   (field offset=8
  // CHECK-32-NEXT:     (reference kind=strong refcounting=native))
  // CHECK-32-NEXT:   (field offset=12
  // CHECK-32-NEXT:     (single_payload_enum size=4 alignment=4 stride=4 num_extra_inhabitants=4095 bitwise_takable=1
  // CHECK-32-NEXT:       (case name=some index=0 offset=0
  // CHECK-32-NEXT:         (reference kind=strong refcounting=native))
  // CHECK-32-NEXT:       (case name=none index=1))))
  @_optimize(none)
  func arity1Capture2() -> (Int) -> () {
   let x: C? = C()
   let closure = { (i: Int) in 
     print(self)
     print(x)
   }
   reflect(function: closure)
   return closure
  }

  // CHECK: Reflecting an object.
  // CHECK: Type reference:
  // CHECK-NEXT: (builtin Builtin.NativeObject)
  
  // CHECK-64: Type info:
  // CHECK-64-NEXT: (closure_context size=40 alignment=8 stride=40 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:   (field offset=16
  // CHECK-64-NEXT:     (reference kind=strong refcounting=native))
  // CHECK-64-NEXT:   (field offset=24
  // CHECK-64-NEXT:     (tuple size=16 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:       (field offset=0
  // CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:           (field name=_value offset=0
  // CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
  // CHECK-64-NEXT:       (field offset=8
  // CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:           (field name=_value offset=0
  // CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))

  // CHECK-32: Type info:
  // CHECK-32-NEXT: (closure_context size=32 alignment=8 stride=32 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:   (field offset=8
  // CHECK-32-NEXT:     (reference kind=strong refcounting=native))
  // CHECK-32-NEXT:   (field offset=16
  // CHECK-32-NEXT:     (tuple size=16 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:       (field offset=0
  // CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:           (field name=_value offset=0
  // CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
  // CHECK-32-NEXT:       (field offset=8
  // CHECK-32-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:           (field name=_value offset=0
  // CHECK-32-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))
  @_optimize(none)
  func arity2Capture2() -> (Int, String) -> () {
   let pair = (999, 1010.2)
   let closure = { (i: Int, s: String) in
     print(self)
     print(pair)
   }

   reflect(function: closure)
   return closure
  }

  // CHECK: Reflecting an object.
  // CHECK: Type reference:
  // CHECK-NEXT: (builtin Builtin.NativeObject)
 
  // CHECK-64: Type info:
  // CHECK-64-NEXT: (closure_context size=40 alignment=8 stride=40 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:   (field offset=16
  // CHECK-64-NEXT:     (reference kind=strong refcounting=native))
  // CHECK-64-NEXT:   (field offset=24
  // CHECK-64-NEXT:     (tuple size=16 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:       (field offset=0
  // CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:           (field name=_value offset=0
  // CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1))))
  // CHECK-64-NEXT:       (field offset=8
  // CHECK-64-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-64-NEXT:           (field name=_value offset=0
  // CHECK-64-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))

  // CHECK-32: Type info:
  // CHECK-32-NEXT: (closure_context size=32 alignment=8 stride=32 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:   (field offset=8
  // CHECK-32-NEXT:     (reference kind=strong refcounting=native))
  // CHECK-32-NEXT:   (field offset=16
  // CHECK-32-NEXT:     (tuple size=16 alignment=8 stride=16 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:       (field offset=0
  // CHECK-32-NEXT:         (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:           (field name=_value offset=0
  // CHECK-32-NEXT:             (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1))))
  // CHECK-32-NEXT:       (field offset=8
  // CHECK-32-NEXT:         (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1
  // CHECK-32-NEXT:           (field name=_value offset=0
  // CHECK-32-NEXT:             (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0 bitwise_takable=1)))))))
  @_optimize(none)
  func arity3Capture2() -> (Int, String, AnyObject?) -> () {
   let pair = (999, 1010.2)
   let closure = { (i: Int, s: String, a: AnyObject?) in
     print(self)
     print(pair)
   }

   reflect(function: closure)
   return closure
  }
}

let cc = CapturingClass()
_ = cc.arity0Capture1()
_ = cc.arity1Capture1()
_ = cc.arity2Capture1()
_ = cc.arity3Capture1()

_ = cc.arity0Capture2()
_ = cc.arity1Capture2()
_ = cc.arity2Capture2()
_ = cc.arity3Capture2()

reflect(function: C().captureWeakSelf())
// CHECK: Reflecting an object.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (builtin Builtin.NativeObject)

// CHECK-64:        Type info:
// CHECK-64-NEXT: (closure_context size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=0
// CHECK-64-NEXT:   (field offset=16
// CHECK-64-NEXT:     (reference kind=weak refcounting=native)))

// CHECK-32:        Type info:
// CHECK-32-NEXT: (closure_context size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=0
// CHECK-32-NEXT:   (field offset=8
// CHECK-32-NEXT:     (reference kind=weak refcounting=native)))

reflect(function: C().captureUnownedSelf())
// CHECK: Reflecting an object.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (builtin Builtin.NativeObject)

// CHECK-64:        Type info:
// CHECK-64-NEXT: (closure_context size=24 alignment=8 stride=24 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-64-NEXT:   (field offset=16
// CHECK-64-NEXT:     (reference kind=unowned refcounting=native)))

// CHECK-32:        Type info:
// CHECK-32-NEXT: (closure_context size=12 alignment=4 stride=12 num_extra_inhabitants=0 bitwise_takable=1
// CHECK-32-NEXT:   (field offset=8
// CHECK-32-NEXT:     (reference kind=unowned refcounting=native)))

doneReflecting()
