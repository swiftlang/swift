// {"kind":"typecheck","signature":"swift::IterableDeclContext::addMember(swift::Decl*, swift::Decl*, bool)","signatureAssert":"Assertion failed: (member->getDeclContext() == static_cast<DeclContext *>(nominal) && \"Added member to the wrong context\"), function addMember","signatureNext":"SwiftDeclConverter::VisitRecordDecl"}
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: not --crash %target-swift-frontend -typecheck -cxx-interoperability-mode=default -I %t/Inputs %t/main.swift

//--- Inputs/module.modulemap
module CxxMod {
  header "test.h"
  requires cplusplus
}

//--- Inputs/test.h
struct A {
  A();
};
struct B {
  friend A::A();
};

//--- main.swift
import CxxMod
func f(_: some A, _: B) {}
