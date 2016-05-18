// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift -Xfrontend -enable-reflection-metadata -Xfrontend -enable-reflection-names -lswiftSwiftReflectionTest %s -o %t/functions
// RUN: %target-run %target-swift-reflection-test %t/functions 2>&1 | FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize
// REQUIRES: objc_interop

// rdar://problem/26230879
// REQUIRES: OS=macosx

/*
   This file pokes at the swift_reflection_infoForInstance() API
   of the SwiftRemoteMirror library.

   It tests introspection of function closure contexts.

   - See also: SwiftReflectionTest.reflect(function:)
*/

import SwiftReflectionTest

func concrete(x: Int, y: Any) {
  reflect(function: {print(x)})
// CHECK:         Type reference:
// CHECK-NEXT:    (builtin Builtin.NativeObject)

// CHECK-32:      Type info:
// CHECK-32-NEXT: (closure_context size=16 alignment=4 stride=16 num_extra_inhabitants=0
// CHECK-32-NEXT:   (field offset=12
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0)))))

// CHECK-64:      Type info:
// CHECK-64-NEXT: (closure_context size=24 alignment=8 stride=24 num_extra_inhabitants=0
// CHECK-64-NEXT:   (field offset=16
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0)))))

  // FIXME: Need @box reflection -- here the context is a single boxed value
  reflect(function: {print(y)})
// CHECK:         Type reference:
// CHECK-NEXT:    (builtin Builtin.NativeObject)

// CHECK-32:      Type info:
// CHECK-32-NEXT: <null type info>

// CHECK-64:      Type info:
// CHECK-64-NEXT: <null type info>
}

concrete(x: 10, y: true)

protocol P {}
extension Int : P {}

class C {}

func generic<T : P, U, V : C>(x: T, y: U, z: V, i: Int) {
  reflect(function: {print(i)})
// CHECK:         Type reference:
// CHECK-NEXT:    (builtin Builtin.NativeObject)

// CHECK-32:      Type info:
// CHECK-32-NEXT: (closure_context size=16 alignment=4 stride=16 num_extra_inhabitants=0
// CHECK-32-NEXT:   (field offset=12
// CHECK-32-NEXT:     (struct size=4 alignment=4 stride=4 num_extra_inhabitants=0
// CHECK-32-NEXT:       (field offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0)))))

// CHECK-64:      Type info:
// CHECK-64-NEXT: (closure_context size=24 alignment=8 stride=24 num_extra_inhabitants=0
// CHECK-64-NEXT:   (field offset=16
// CHECK-64-NEXT:     (struct size=8 alignment=8 stride=8 num_extra_inhabitants=0
// CHECK-64-NEXT:       (field offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=0)))))

  reflect(function: {print(x); print(y); print(z)})
// CHECK:         Type reference:
// CHECK-NEXT:    (builtin Builtin.NativeObject)

// CHECK-32:      Type info:
// CHECK-32-NEXT: (closure_context size=40 alignment=4 stride=40 num_extra_inhabitants=0
// CHECK-32-NEXT:   (field offset=28
// CHECK-32-NEXT:     (reference kind=strong refcounting=native))
// CHECK-32-NEXT:   (field offset=32
// CHECK-32-NEXT:     (reference kind=strong refcounting=native))
// CHECK-32-NEXT:   (field offset=36
// CHECK-32-NEXT:     (reference kind=strong refcounting=native)))

// CHECK-64:      Type info:
// CHECK-64-NEXT: (closure_context size=72 alignment=8 stride=72 num_extra_inhabitants=0
// CHECK-64-NEXT:   (field offset=48
// CHECK-64-NEXT:     (reference kind=strong refcounting=native))
// CHECK-64-NEXT:   (field offset=56
// CHECK-64-NEXT:     (reference kind=strong refcounting=native))
// CHECK-64-NEXT:   (field offset=64
// CHECK-64-NEXT:     (reference kind=strong refcounting=native)))
}

generic(x: 10, y: "", z: C(), i: 101)

class GC<A, B, C> {}

func genericWithSources<A, B, C>(a: A, b: B, c: C, gc: GC<A, B, C>) {
  reflect(function: {print(a); print(b); print(c); print(gc)})
// CHECK:         Type reference:
// CHECK-NEXT:    (builtin Builtin.NativeObject)

// CHECK-32:      Type info:
// CHECK-32-NEXT: (closure_context size=28 alignment=4 stride=28 num_extra_inhabitants=0
// CHECK-32-NEXT:   (field offset=12
// CHECK-32-NEXT:     (reference kind=strong refcounting=native))
// CHECK-32-NEXT:   (field offset=16
// CHECK-32-NEXT:     (reference kind=strong refcounting=native))
// CHECK-32-NEXT:   (field offset=20
// CHECK-32-NEXT:     (reference kind=strong refcounting=native))
// CHECK-32-NEXT:   (field offset=24
// CHECK-32-NEXT:     (reference kind=strong refcounting=native)))

// CHECK-64:      Type info:
// CHECK-64-NEXT: (closure_context size=48 alignment=8 stride=48 num_extra_inhabitants=0
// CHECK-64-NEXT:   (field offset=16
// CHECK-64-NEXT:     (reference kind=strong refcounting=native))
// CHECK-64-NEXT:   (field offset=24
// CHECK-64-NEXT:     (reference kind=strong refcounting=native))
// CHECK-64-NEXT:   (field offset=32
// CHECK-64-NEXT:     (reference kind=strong refcounting=native))
// CHECK-64-NEXT:   (field offset=40
// CHECK-64-NEXT:     (reference kind=strong refcounting=native)))
}

genericWithSources(a: (), b: ((), ()), c: ((), (), ()), gc: GC<(), ((), ()), ((), (), ())>())

doneReflecting()
