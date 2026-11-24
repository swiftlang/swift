// RUN: %target-run-simple-swift(-enable-experimental-feature RawLayout -enable-builtin-module -Xfrontend -disable-availability-checking) | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: swift_feature_RawLayout
// REQUIRES: synchronization

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Builtin
import Synchronization

@_rawLayout(like: T)
struct Cell<T: ~Copyable>: ~Copyable {}

struct Foo<T>: ~Copyable {
  let cell = Cell<T>()
  let myValue = 123
}

@_rawLayout(like: T, movesAsLike)
struct RealCell<T: ~Copyable>: ~Copyable {}

@_rawLayout(like: T, movesAsLike)
struct ActuallyRealCell<T: ~Copyable>: ~Copyable {
  deinit {
    UnsafeMutablePointer<T>(Builtin.addressOfRawLayout(self)).deinitialize(count: 1)
  }
}

@_rawLayout(like: Int)
struct RawInt: ~Copyable {}

func something<T>(_ x: borrowing Foo<T>) -> Int {
  x.myValue
}

// CHECK: 123
print(something(Foo<Bool>()))

// CHECK: 123
print(something(Foo<Int>()))

class MyCls {
  init() { print("MyCls.init" )}
  deinit { print("MyCls.deinit") }
}

// This function is intentionally testing runtime behavior, so we inspect
// the raw value witness flags instead of using compiler builtins to check
// type traits. 
// The unsafe pointer chasing here ought to be opaque to the compiler, but
// just as a hedge against any future inlining or constant-folding capabilities
// let's prevent inlining or other optimizations as much as we can here.
@inline(never)
@_optimize(none)
func valueWitnessFlags(for type: any ~Copyable.Type) -> UInt {
	let metadataPtr = unsafeBitCast(type, to: UnsafePointer<UnsafeRawPointer>.self)
	let vwPtr = metadataPtr[-1].assumingMemoryBound(to: UInt.self)
	return vwPtr[10]
}

func isPOD(_ type: any ~Copyable.Type) -> Bool {
  // 0x00010000: is not pod
  // flag must be 0 to be pod
  valueWitnessFlags(for: type) & 0x00010000 == 0
}

// CHECK: start
print("start")

// CHECK-NEXT: true
print(isPOD(RawInt.self))

// CHECK-NEXT: true
print(isPOD(Cell<Int>.self))

// Note: Even though 'String' is NOT POD, we can't assume a value of 'Cell' is
//       ever fully initialized.
// CHECK-NEXT: true
print(isPOD(Cell<String>.self))

// CHECK-NEXT: true
print(isPOD(RealCell<Int>.self))

// CHECK-NEXT: false
print(isPOD(RealCell<String>.self))

// CHECK-NEXT: false
print(isPOD(ActuallyRealCell<Int>.self))

// CHECK-NEXT: false
print(isPOD(ActuallyRealCell<String>.self))

// FIXME: This could be POD on some platforms...
// CHECK-NEXT: false
print(isPOD(Mutex<Int>.self))

// CHECK-NEXT: false
print(isPOD(Mutex<MyCls>.self))

// FIXME: This is POD, but 'Atomic' itself has a deinit that kind of defeats the
//        compiler right now...
// CHECK-NEXT: false
print(isPOD(Atomic<Int>.self))

// CHECK: done
print("done")
