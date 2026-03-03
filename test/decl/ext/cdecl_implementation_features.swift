// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource) -typecheck %s -import-objc-header %S/Inputs/objc_implementation.h > %t 2>&1
// RUN: %FileCheck --input-file %t --check-prefixes YES,CHECK %s
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource) -typecheck %s -import-objc-header %S/Inputs/objc_implementation.h > %t 2>&1
// RUN: %FileCheck --input-file %t --check-prefixes YES,C,CHECK %s

// YES-DAG: cdecl_implementation_features.swift:[[@LINE+1]]:{{[0-9]+}}: warning: global function 'CImplFunc1' of type '(Double) -> ()' does not match type '(Int32) -> Void' declared by the header; this will become an error after adopting '@implementation'
@_objcImplementation @_cdecl("CImplFunc1") func CImplFunc1(_: Double) {}

// YES-DAG: cdecl_implementation_features.swift:[[@LINE+2]]:{{[0-9]+}}: error: global function 'CImplFunc2' of type '(Double) -> ()' does not match type '(Int32) -> Void' declared by the header{{$}}
// YES-NOT: cdecl_implementation_features.swift:[[@LINE+1]]:{{[0-9]+}}: error: 'implementation' attribute is only valid when experimental feature CImplementation is enabled
@implementation @_cdecl("CImplFunc2") func CImplFunc2(_: Double) {}
