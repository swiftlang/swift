// RUN: %target-swift-emit-ir %s -I %S/Inputs -cxx-interoperability-mode=default -disable-availability-checking | %FileCheck %s

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

// TODO: optional pointers are not yet supported but they should be
// func takesCPtr() { takesValue(intPtr) }
func takesCPtr() { let swiftPtr = intPtr!; takesValue(swiftPtr) }

// TODO: function pointers are not yet supported but they should be
// func takesCFnPtr() { takesValue(functionPtr) }
