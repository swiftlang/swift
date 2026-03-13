// This regression test was originally written because the Swift compiler would
// crash when it tried to instantiate C++ function templates with various types.
// In particular, instantiating them with Swift types was problematic.
//
// Aside from validating that we can successfully instantiate function templates
// with basic primitive types (e.g., Bool, Int32) and imported C/ObjC/C++ types,
// this regression test aims to document the current status of C++ function
// template instantiation.
//
// With the following, we check the happy path, that we _can_ successfully emit
// SIL for certain template instantiations:
// RUN: %target-swift-emit-ir %s -I %S/Inputs -cxx-interoperability-mode=default -disable-availability-checking | %FileCheck %s
//
// Some instantiations still cause Swift compiler crashes. These are documented
// with "FIXME" comments.

import FunctionTemplates

func takesInt32(x: Int32) { takesValue(x) }
// CHECK: define {{.*}} void @{{.*}}takesInt32{{.*}}

func takesBool(x: Bool) { takesValue(x) }
// CHECK: define {{.*}} void @{{.*}}takesBool{{.*}}

func takesTrue() { takesValue(true) }
// CHECK: define {{.*}} void @{{.*}}takesTrue{{.*}}

func takesFalse() { takesValue(false) }
// CHECK: define {{.*}} void @{{.*}}takesFalse{{.*}}

func takesPlainStruct(x: PlainStruct) { takesValue(x) }
// CHECK: define {{.*}} void @{{.*}}takesPlainStruct{{.*}}

func takesCxxClass(x: CxxClass) { takesValue(x) }
// CHECK: define {{.*}} void @{{.*}}takesCxxClass{{.*}}

func takesFRT(x: FRT) { takesValue(x) }
// CHECK: define {{.*}} void @{{.*}}takesFRT{{.*}}

func takesSIMD(x: SIMD32<Int32>) { takesValue(x) }
// CHECK: define {{.*}} void @{{.*}}takesSIMD{{.*}}

func takesPtrToStruct(x: UnsafePointer<PlainStruct>) { takesValue(x) }
// CHECK: define {{.*}} void @{{.*}}takesPtrToStruct{{.*}}

func takesPtrToClass(x: UnsafePointer<CxxClass>) { takesValue(x) }
// CHECK: define {{.*}} void @{{.*}}takesPtrToClass{{.*}}

func takesPtrToFRT(x: UnsafePointer<FRT>) { takesValue(x) }
// CHECK: define {{.*}} void @{{.*}}takesPtrToFRT{{.*}}

func takesMutPtrToStruct(x: UnsafeMutablePointer<PlainStruct>) { takesValue(x) }
// CHECK: define {{.*}} void @{{.*}}takesMutPtrToStruct{{.*}}

func takesMutPtrToClass(x: UnsafeMutablePointer<CxxClass>) { takesValue(x) }
// CHECK: define {{.*}} void @{{.*}}takesMutPtrToClass{{.*}}

func takesMutPtrToFRT(x: UnsafeMutablePointer<FRT>) { takesValue(x) }
// CHECK: define {{.*}} void @{{.*}}takesMutPtrToFRT{{.*}}

func takesCPtr() {
  takesValue(intPtr)

  // It's fine if we dereference it, though
  takesValue(intPtr!)
}

func takesCFnPtr() {
  takesValue(get42) // function symbol
  // FIXME: optional pointers are not yet supported but they should be; this crashes
  // takesValue(functionPtrGet42)
  takesValue(functionPtrGet42!) // dereferenced nullable function pointer
  takesValue(nonNullFunctionPtrGet42) // non-null function symbol
}

func takesRecursively() { takesValue(takesRecursively) }
func takesRecursiveClosure() { takesValue({() in takesRecursiveClosure()}) }
func takesSwiftClosure() { takesValue({() in ()}) }
func takesTakesTrue() { takesValue(takesTrue) }

func takesSwiftClosureReturningBool() { takesValue({() -> Bool in true}) }
func takesSwiftClosureTakingBool() { takesValue({(x: Bool) in ()}) }
func takesTakesBool() { takesValue(takesBool) }

func takesSwiftClosureReturningPlainStruct() { takesValue({() -> PlainStruct in PlainStruct(x: 42)}) }
func takesSwiftClosureTakingPlainStruct() { takesValue({(x: PlainStruct) in takesValue(x)}) }
func takesTakesPlainStruct() { takesValue(takesPlainStruct) }

func takesSwiftClosureReturningCxxClass() { takesValue({() -> CxxClass in CxxClass(x: 42)}) }
func takesSwiftClosureTakingCxxClass() { takesValue({(x: CxxClass) in takesValue(x)}) }
func takesTakesCxxClass() { takesValue(takesCxxClass) }

func takesSwiftClosureReturningFRT() { takesValue({() -> FRT in FRT()}) }
func takesSwiftClosureTakingFRT() { takesValue({(x: FRT) in takesValue(x)}) }

func takesTakesFRT() { takesValue(takesFRT) }
