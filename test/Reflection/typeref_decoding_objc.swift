// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %S/Inputs/ObjectiveCTypes.swift -parse-as-library -emit-module -emit-library -module-name TypesToReflect -o %t/libTypesToReflect.%target-dylib-extension
// RUN: %target-swift-reflection-dump -binary-filename %t/libTypesToReflect.%target-dylib-extension | FileCheck %s --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK
// REQUIRES: objc_interop

// CHECK: FIELDS:
// CHECK: =======
// CHECK: TypesToReflect.OC
// CHECK: -----------------
// CHECK: nsObject: __ObjC.NSObject
// CHECK: (class __ObjC.NSObject)

// CHECK: nsString: __ObjC.NSString
// CHECK: (class __ObjC.NSString)

// CHECK: cfString: __ObjC.CFString
// CHECK: (class __ObjC.CFString)

// CHECK: aBlock: @convention(block) () -> ()
// CHECK: (function convention=block
// CHECK:   (tuple))

// CHECK: TypesToReflect.GenericOC
// CHECK: ------------------------
// CHECK: ocnss: TypesToReflect.GenericOC<__ObjC.NSString>
// CHECK: (bound_generic_class TypesToReflect.GenericOC
// CHECK:   (class __ObjC.NSString))

// CHECK: occfs: TypesToReflect.GenericOC<__ObjC.CFString>
// CHECK: (bound_generic_class TypesToReflect.GenericOC
// CHECK:   (class __ObjC.CFString))

// CHECK: TypesToReflect.HasObjCClasses
// CHECK: -----------------------------
// CHECK: url: __ObjC.NSURL
// CHECK: (class __ObjC.NSURL)

// CHECK: integer: Swift.Int
// CHECK: (struct Swift.Int)

// CHECK: rect: __C.CGRect
// CHECK: (struct __C.CGRect)

// CHECK: __ObjC.NSBundle
// CHECK: ---------------
// CHECK: __ObjC.NSURL
// CHECK: ------------
// CHECK: __ObjC.NSCoding
// CHECK: ---------------

// CHECK: ASSOCIATED TYPES:
// CHECK: =================

// CHECK: BUILTIN TYPES:
// CHECK: ==============

// CHECK-32: - __C.CGRect:
// CHECK-32: Size: 16
// CHECK-32: Alignment: 4
// CHECK-32: Stride: 16
// CHECK-32: NumExtraInhabitants: 0

// CHECK-64: Size: 32
// CHECK-64: Alignment: 8
// CHECK-64: Stride: 32
// CHECK-64: NumExtraInhabitants: 0

// CHECK: CAPTURE DESCRIPTORS:
// CHECK: ====================
// CHECK: - Capture types:
// CHECK: (struct Swift.StaticString)
// CHECK: (struct Swift.StaticString)
// CHECK: (struct Swift.UInt)
// CHECK: (struct Swift.UInt)
// CHECK: - Metadata sources:

// CHECK: - Capture types:
// CHECK: (function
// CHECK:   (tuple))
// CHECK: - Metadata sources:

// CHECK: - Capture types:
// CHECK: (struct Swift.StaticString)
// CHECK: (struct Swift.StaticString)
// CHECK: (struct Swift.UInt)
// CHECK: (struct Swift.UInt)
// CHECK: - Metadata sources:

// CHECK: - Capture types:
// CHECK: (function
// CHECK:   (tuple))
// CHECK: - Metadata sources:

// CHECK: - Capture types:
// CHECK: (class __ObjC.NSBundle)
// CHECK: (protocol __ObjC.NSCoding)
// CHECK: - Metadata sources:

// CHECK: - Capture types:
// CHECK: (struct Swift.StaticString)
// CHECK: (bound_generic_struct Swift.UnsafeBufferPointer
// CHECK:   (struct Swift.UInt8))
// CHECK: (struct Swift.UInt)
// CHECK: (struct Swift.UInt)
// CHECK: - Metadata sources:

// CHECK: - Capture types:
// CHECK: (function
// CHECK:   (tuple))
// CHECK: - Metadata sources:

// CHECK: - Capture types:
// CHECK: (bound_generic_struct Swift.UnsafeBufferPointer
// CHECK:   (struct Swift.UInt8))
// CHECK: (bound_generic_struct Swift.UnsafeBufferPointer
// CHECK:   (struct Swift.UInt8))
// CHECK: (struct Swift.UInt)
// CHECK: (struct Swift.UInt)
// CHECK: - Metadata sources:

// CHECK: - Capture types:
// CHECK: (function
// CHECK:   (tuple))
// CHECK: - Metadata sources:
