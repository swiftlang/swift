// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: echo "public var dummyVar = Int()" | %target-swift-frontend -module-name DummyModule -emit-module -o %t -
// RUN: %target-swift-frontend -typecheck -verify -I %t %s

// REQUIRES: can_import

#if canImport(DummyModule)
print(DummyModule.dummyVar) // expected-error {{use of unresolved identifier 'DummyModule'}}
print(dummyVar) // expected-error {{use of unresolved identifier 'dummyVar'}}
#endif
