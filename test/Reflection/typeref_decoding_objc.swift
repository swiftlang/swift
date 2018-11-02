// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/ObjectiveCTypes.swift -parse-as-library -emit-module -emit-library -module-name TypesToReflect -o %t/libTypesToReflect.%target-dylib-extension
// RUN: %target-swift-reflection-dump -binary-filename %t/libTypesToReflect.%target-dylib-extension | %FileCheck %s --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK
// REQUIRES: objc_interop

// Disable asan builds until we build swift-reflection-dump and the reflection library with the same compile: rdar://problem/30406870
// REQUIRES: no_asan

// CHECK: FIELDS:
// CHECK: =======
// CHECK: TypesToReflect.OC
// CHECK: -----------------
// CHECK: nsObject: __C.NSObject
// CHECK: (objective_c_class name=NSObject)

// CHECK: nsString: __C.NSString
// CHECK: (objective_c_class name=NSString)

// CHECK: cfString: __C.CFStringRef
// CHECK: (alias __C.CFStringRef)

// CHECK: aBlock: @convention(block) () -> ()
// CHECK: (function convention=block
// CHECK:   (tuple))

// CHECK: ocnss: TypesToReflect.GenericOC<__C.NSString>
// CHECK: (bound_generic_class TypesToReflect.GenericOC
// CHECK:   (objective_c_class name=NSString))

// CHECK: occfs: TypesToReflect.GenericOC<__C.CFStringRef>
// CHECK: (bound_generic_class TypesToReflect.GenericOC
// CHECK:   (alias __C.CFStringRef))

// CHECK: TypesToReflect.GenericOC
// CHECK: ------------------------

// CHECK: TypesToReflect.HasObjCClasses
// CHECK: -----------------------------
// CHECK: url: __C.NSURL
// CHECK: (objective_c_class name=NSURL)

// CHECK: integer: Swift.Int
// CHECK: (struct Swift.Int)

// CHECK: rect: __C.CGRect
// CHECK: (struct __C.CGRect)

// CHECK: TypesToReflect.OP
// CHECK: -----------------

// CHECK: ASSOCIATED TYPES:
// CHECK: =================

// CHECK: BUILTIN TYPES:
// CHECK: ==============

// CHECK-32: - __C.CGRect:
// CHECK-32: Size: 16
// CHECK-32: Alignment: 4
// CHECK-32: Stride: 16
// CHECK-32: NumExtraInhabitants: 0

// CHECK-64: - __C.CGRect:
// CHECK-64: Size: 32
// CHECK-64: Alignment: 8
// CHECK-64: Stride: 32
// CHECK-64: NumExtraInhabitants: 0

// CHECK:      CAPTURE DESCRIPTORS:
// CHECK-NEXT: ====================

// CHECK:      - Capture types:
// CHECK-NEXT: (objective_c_class name=NSBundle)
// CHECK-NEXT: (protocol_composition
// CHECK-NEXT:   (objective_c_protocol name=NSCoding))
// CHECK-NEXT: - Metadata sources:
