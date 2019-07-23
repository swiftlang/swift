// RUN: %empty-directory(%t)
//
// RUN: echo "public var dummyVar = Int()" | %target-swift-frontend -module-name DummyModule -emit-module -o %t -
// RUN: %target-swift-frontend -typecheck -verify -I %t %s

#if canImport(DummyModule)
print(DummyModule.dummyVar) // expected-error {{use of unresolved identifier 'DummyModule'}}
print(dummyVar) // expected-error {{use of unresolved identifier 'dummyVar'}}
#endif
