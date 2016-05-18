// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %S/Inputs/ObjectiveCTypes.swift -parse-as-library -emit-module -emit-library -module-name TypesToReflect -Xfrontend -enable-reflection-metadata -Xfrontend -enable-reflection-names -o %t/libTypesToReflect.%target-dylib-extension
// RUN: %target-swift-reflection-dump -binary-filename %t/libTypesToReflect.%target-dylib-extension | FileCheck %s --check-prefix=CHECK-%target-ptrsize
// REQUIRES: objc_interop

// CHECK-32: FIELDS:
// CHECK-32: =======
// CHECK-32: TypesToReflect.OC
// CHECK-32: -----------------
// CHECK-32: TypesToReflect.GenericOC
// CHECK-32: ------------------------
// CHECK-32: TypesToReflect.HasObjCClasses
// CHECK-32: -----------------------------
// CHECK-32: url: __ObjC.NSURL
// CHECK-32: (class __ObjC.NSURL)

// CHECK-32: integer: Swift.Int
// CHECK-32: (struct Swift.Int)

// CHECK-32: __ObjC.NSURL
// CHECK-32: ------------

// CHECK-32: ASSOCIATED TYPES:
// CHECK-32: =================
// CHECK-32: - TypesToReflect.OC : Swift.AnyObject
// CHECK-32: - TypesToReflect.OC : __ObjC.NSObjectProtocol
// CHECK-32: - TypesToReflect.OC : Swift.Equatable
// CHECK-32: - TypesToReflect.OC : Swift.Hashable
// CHECK-32: - TypesToReflect.OC : Swift.CVarArg
// CHECK-32: - TypesToReflect.OC : Swift.CustomStringConvertible
// CHECK-32: - TypesToReflect.OC : Swift.CustomDebugStringConvertible
// CHECK-32: - TypesToReflect.GenericOC : Swift.AnyObject
// CHECK-32: - TypesToReflect.GenericOC : __ObjC.NSObjectProtocol
// CHECK-32: - TypesToReflect.GenericOC : Swift.Equatable
// CHECK-32: - TypesToReflect.GenericOC : Swift.Hashable
// CHECK-32: - TypesToReflect.GenericOC : Swift.CVarArg
// CHECK-32: - TypesToReflect.GenericOC : Swift.CustomStringConvertible
// CHECK-32: - TypesToReflect.GenericOC : Swift.CustomDebugStringConvertible
// CHECK-32: - TypesToReflect.HasObjCClasses : Swift.AnyObject

// CHECK-32: BUILTIN TYPES:
// CHECK-32: ==============

// CHECK-32: - __ObjC.NSURL:
// CHECK-32: Size: 4
// CHECK-32: Alignment: 4
// CHECK-32: Stride: 4
// CHECK-32: NumExtraInhabitants: 4096

// CHECK-32: CAPTURE DESCRIPTORS:
// CHECK-32: ====================
// CHECK-32: - Capture types:
// CHECK-32: (struct Swift.StaticString)
// CHECK-32: (struct Swift.StaticString)
// CHECK-32: (struct Swift.UInt)
// CHECK-32: (struct Swift.UInt)
// CHECK-32: - Metadata sources:

// CHECK-32: - Capture types:
// CHECK-32: (function
// CHECK-32:   (tuple))
// CHECK-32: - Metadata sources:

// CHECK-32: - Capture types:
// CHECK-32: (struct Swift.StaticString)
// CHECK-32: (bound_generic_struct Swift.UnsafeBufferPointer
// CHECK-32:   (struct Swift.UInt8))
// CHECK-32: (struct Swift.UInt)
// CHECK-32: (struct Swift.UInt)
// CHECK-32: - Metadata sources:

// CHECK-32: - Capture types:
// CHECK-32: (function
// CHECK-32:   (tuple))
// CHECK-32: - Metadata sources:

// CHECK-32: - Capture types:
// CHECK-32: (bound_generic_struct Swift.UnsafeBufferPointer
// CHECK-32:   (struct Swift.UInt8))
// CHECK-32: (bound_generic_struct Swift.UnsafeBufferPointer
// CHECK-32:   (struct Swift.UInt8))
// CHECK-32: (struct Swift.UInt)
// CHECK-32: (struct Swift.UInt)
// CHECK-32: - Metadata sources:

// CHECK-32: - Capture types:
// CHECK-32: (function
// CHECK-32:   (tuple))
// CHECK-32: - Metadata sources:


// CHECK-64: FIELDS:
// CHECK-64: =======
// CHECK-64: TypesToReflect.OC
// CHECK-64: -----------------
// CHECK-64: TypesToReflect.GenericOC
// CHECK-64: ------------------------
// CHECK-64: TypesToReflect.HasObjCClasses
// CHECK-64: -----------------------------
// CHECK-64: url: __ObjC.NSURL
// CHECK-64: (class __ObjC.NSURL)

// CHECK-64: integer: Swift.Int
// CHECK-64: (struct Swift.Int)

// CHECK-64: __ObjC.NSURL
// CHECK-64: ------------

// CHECK-64: ASSOCIATED TYPES:
// CHECK-64: =================
// CHECK-64: - TypesToReflect.OC : Swift.AnyObject
// CHECK-64: - TypesToReflect.OC : __ObjC.NSObjectProtocol
// CHECK-64: - TypesToReflect.OC : Swift.Equatable
// CHECK-64: - TypesToReflect.OC : Swift.Hashable
// CHECK-64: - TypesToReflect.OC : Swift.CVarArg
// CHECK-64: - TypesToReflect.OC : Swift.CustomStringConvertible
// CHECK-64: - TypesToReflect.OC : Swift.CustomDebugStringConvertible
// CHECK-64: - TypesToReflect.GenericOC : Swift.AnyObject
// CHECK-64: - TypesToReflect.GenericOC : __ObjC.NSObjectProtocol
// CHECK-64: - TypesToReflect.GenericOC : Swift.Equatable
// CHECK-64: - TypesToReflect.GenericOC : Swift.Hashable
// CHECK-64: - TypesToReflect.GenericOC : Swift.CVarArg
// CHECK-64: - TypesToReflect.GenericOC : Swift.CustomStringConvertible
// CHECK-64: - TypesToReflect.GenericOC : Swift.CustomDebugStringConvertible
// CHECK-64: - TypesToReflect.HasObjCClasses : Swift.AnyObject

// CHECK-64: BUILTIN TYPES:
// CHECK-64: ==============

// CHECK-64: - __ObjC.NSURL:
// CHECK-64: Size: 8
// CHECK-64: Alignment: 8
// CHECK-64: Stride: 8
// CHECK-64: NumExtraInhabitants: 2147483647

// CHECK-64: CAPTURE DESCRIPTORS:
// CHECK-64: ====================
// CHECK-64: - Capture types:
// CHECK-64: (struct Swift.StaticString)
// CHECK-64: (struct Swift.StaticString)
// CHECK-64: (struct Swift.UInt)
// CHECK-64: (struct Swift.UInt)
// CHECK-64: - Metadata sources:

// CHECK-64: - Capture types:
// CHECK-64: (function
// CHECK-64:   (tuple))
// CHECK-64: - Metadata sources:

// CHECK-64: - Capture types:
// CHECK-64: (struct Swift.StaticString)
// CHECK-64: (bound_generic_struct Swift.UnsafeBufferPointer
// CHECK-64:   (struct Swift.UInt8))
// CHECK-64: (struct Swift.UInt)
// CHECK-64: (struct Swift.UInt)
// CHECK-64: - Metadata sources:

// CHECK-64: - Capture types:
// CHECK-64: (function
// CHECK-64:   (tuple))
// CHECK-64: - Metadata sources:

// CHECK-64: - Capture types:
// CHECK-64: (bound_generic_struct Swift.UnsafeBufferPointer
// CHECK-64:   (struct Swift.UInt8))
// CHECK-64: (bound_generic_struct Swift.UnsafeBufferPointer
// CHECK-64:   (struct Swift.UInt8))
// CHECK-64: (struct Swift.UInt)
// CHECK-64: (struct Swift.UInt)
// CHECK-64: - Metadata sources:

// CHECK-64: - Capture types:
// CHECK-64: (function
// CHECK-64:   (tuple))
// CHECK-64: - Metadata sources:

