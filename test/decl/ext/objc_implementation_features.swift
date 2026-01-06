// REQUIRES: objc_interop

// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource) -typecheck %s -import-objc-header %S/Inputs/objc_implementation.h >%t.txt 2>&1
// RUN: %FileCheck %s --input-file %t.txt

// CHECK-DAG: objc_implementation_features.swift:[[@LINE+1]]:{{[0-9]+}}: warning: '@_objcImplementation' is deprecated; use '@implementation' instead
@_objcImplementation(EmptyCategory) extension ObjCClass {}

// CHECK-DAG: objc_implementation_features.swift:[[@LINE+2]]:{{[0-9]+}}: warning: extension for main class interface does not provide all required implementations; this will become an error after adopting '@implementation'
// CHECK-NOT: objc_implementation_features.swift:[[@LINE+1]]:{{[0-9]+}}: warning: '@_objcImplementation' is deprecated; use '@implementation' instead
@_objcImplementation extension ObjCSubclass {}

// CHECK-DAG: objc_implementation_features.swift:[[@LINE+2]]:{{[0-9]+}}: error: extension for main class interface does not provide all required implementations{{$}}
// CHECK-NOT: objc_implementation_features.swift:[[@LINE+1]]:{{[0-9]+}}: error: 'implementation' attribute is only valid when experimental feature ObjCImplementation is enabled
@objc @implementation extension ObjCBasicInitClass {}
