// REQUIRES: objc_interop

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource) -typecheck -verify -verify-ignore-unrelated %s -import-objc-header %S/Inputs/objc_implementation.h -Xcc -Wno-nullability-completeness

@_objcImplementation(EmptyCategory) extension ObjCClass {}
// expected-warning@-1 {{'@_objcImplementation' is deprecated; use '@implementation' instead}}

@_objcImplementation extension ObjCSubclass {}
// expected-warning@-1 {{extension for main class interface does not provide all required implementations; this will become an error after adopting '@implementation'}}
// expected-note@-2 {{missing instance method 'subclassMethod(fromHeader1:)'}}
// expected-note@-3 {{add stub for missing '@implementation' requirement}}

@objc @implementation extension ObjCBasicInitClass {}
// expected-error@-1 {{extension for main class interface does not provide all required implementations}}
// expected-note@-2 {{missing initializer 'init()'}}
// expected-note@-3 {{add stub for missing '@implementation' requirement}}
