// RUN: %target-swift-emit-ir %s -I %S/Inputs -cxx-interoperability-mode=default -disable-availability-checking | %FileCheck %s

import FunctionTemplates

func takesInt32(x: Int32) { takesValue(x) }
// CHECK: define {{.*}} void @{{.*}}10takesInt32{{.*}}

func takesBool(x: Bool) { takesValue(x) }
// CHECK: define {{.*}} void @{{.*}}9takesBool{{.*}}

func takesTrue() { takesValue(true) }
// CHECK: define {{.*}} void @{{.*}}9takesTrue{{.*}}

func takesFalse() { takesValue(false) }
// CHECK: define {{.*}} void @{{.*}}10takesFalse{{.*}}

func takesPlainStruct(x: PlainStruct) { takesValue(x) }
// CHECK: define {{.*}} void @{{.*}}16takesPlainStruct{{.*}}

func takesCxxClass(x: CxxClass) { takesValue(x) }
// CHECK: define {{.*}} void @{{.*}}13takesCxxClass{{.*}}

func takesFRT(x: FRT) { takesValue(x) }
// CHECK: define {{.*}} void @{{.*}}8takesFRT{{.*}}

// func takesUnsafeMutablePointer(x: UnsafeMutablePointer<FRT>) { takesValue(x) } // crashes

// func takesUnsafePointer(x: UnsafePointer<FRT>) { takesValue(x) } // crashes

// func takesAutoreleasingUnsafeMutablePointer(x: AutoreleasingUnsafeMutablePointer<FRT>) { takesValue(x) } // crashes

// func takesUnmanaged(x: Unmanaged<FRT>) { takesValue(x) } // crashes

// func takesOptional(x: FRT?) { takesValue(x) } // crashes

func takesSIMD(x: SIMD32<Int32>) { takesValue(x) }
// CHECK: define {{.*}} void @{{.*}}9takesSIMD{{.*}}
