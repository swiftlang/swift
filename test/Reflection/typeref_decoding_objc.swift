// RUN: rm -rf %t && mkdir -p %t
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
// CHECK: (class __C.NSObject)

// CHECK: nsString: __C.NSString
// CHECK: (class __C.NSString)

// CHECK: cfString: __C.CFString
// CHECK: (class __C.CFString)

// CHECK: aBlock: @convention(block) () -> ()
// CHECK: (function convention=block
// CHECK:   (tuple))

// CHECK: ocnss: TypesToReflect.GenericOC<__C.NSString>
// CHECK: (bound_generic_class TypesToReflect.GenericOC
// CHECK:   (class __C.NSString))

// CHECK: occfs: TypesToReflect.GenericOC<__C.CFString>
// CHECK: (bound_generic_class TypesToReflect.GenericOC
// CHECK:   (class __C.CFString))

// CHECK: TypesToReflect.GenericOC
// CHECK: ------------------------

// CHECK: TypesToReflect.HasObjCClasses
// CHECK: -----------------------------
// CHECK: url: __C.NSURL
// CHECK: (class __C.NSURL)

// CHECK: integer: Swift.Int
// CHECK: (struct Swift.Int)

// CHECK: rect: __C.CGRect
// CHECK: (struct __C.CGRect)

// CHECK: TypesToReflect.OP
// CHECK: -----------------

// CHECK: __C.Bundle
// CHECK: ----------
// CHECK: __C.NSURL
// CHECK: ---------
// CHECK: __C.NSCoding
// CHECK: ------------

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
// CHECK-NEXT: (class __C.Bundle)
// CHECK-NEXT: (protocol __C.NSCoding)
// CHECK-NEXT: - Metadata sources:
