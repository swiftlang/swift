// {"kind":"typecheck","signature":"swift::NormalProtocolConformance::getAssociatedConformance(swift::Type, swift::ProtocolDecl*) const","signatureAssert":"Assertion failed: (!AssociatedConformances[index]), function setAssociatedConformance","signatureNext":"SubstitutionMap::lookupConformance"}
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: not --crash %target-swift-frontend -typecheck -cxx-interoperability-mode=default -I %t/Inputs %t/main.swift

//--- Inputs/module.modulemap
module CxxMod {
  header "test.h"
  requires cplusplus
}

//--- Inputs/test.h
struct I {};
struct S {
  I begin() const;
  I end() const;
};

//--- main.swift
import CxxMod
extension S: Sequence, CxxSequence {}
extension I: UnsafeCxxInputIterator {}
