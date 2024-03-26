// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource) -typecheck %s -import-objc-header %S/Inputs/objc_implementation.h 2>&1 | %FileCheck --check-prefixes NO,CHECK %s
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource) -typecheck %s -import-objc-header %S/Inputs/objc_implementation.h -enable-experimental-feature CImplementation 2>&1 | %FileCheck --check-prefixes YES,C,CHECK %s

// NO-DAG: cdecl_implementation_features.swift:[[@LINE+3]]:{{[0-9]+}}: error: '_objcImplementation' attribute is only valid when experimental feature CImplementation is enabled
// YES-NOT: cdecl_implementation_features.swift:[[@LINE+2]]:{{[0-9]+}}: warning: warning-only '@_objcImplementation' spelling is deprecated; use '@implementation' instead
// TODO: When @implementation @_cdecl stabilizes, YES-NOT on the line above will become YES-DAG
@_objcImplementation @_cdecl("CImplFunc1") func CImplFunc1(_: Double) {}

// NO-DAG: cdecl_implementation_features.swift:[[@LINE+2]]:{{[0-9]+}}: error: 'implementation' attribute is only valid when experimental feature CImplementation is enabled
// YES-NOT: cdecl_implementation_features.swift:[[@LINE+1]]:{{[0-9]+}}: error: 'implementation' attribute is only valid when experimental feature CImplementation is enabled
@implementation @_cdecl("CImplFunc2") func CImplFunc2(_: Double) {}
