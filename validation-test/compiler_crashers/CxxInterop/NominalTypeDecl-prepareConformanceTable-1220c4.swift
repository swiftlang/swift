// {"kind":"typecheck","signature":"swift::NominalTypeDecl::prepareConformanceTable() const","signatureAssert":"Assertion failed: (containsInverseReq && \"this synthesized conformance is invalid\"), function operator()","signatureNext":"NominalTypeDecl::lookupConformance"}
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: not --crash %target-swift-frontend -typecheck -cxx-interoperability-mode=default -module-name M -I %t/Inputs %t/main.swift

//--- Inputs/module.modulemap
module CxxMod {
  header "test.h"
  requires cplusplus
}

//--- Inputs/test.h
struct __attribute__((swift_attr("conforms_to:M.P"))) S {
  S(const S &) = delete;
  S(S &&) = default;
};

//--- main.swift
import CxxMod
protocol P {
}
func f(_: borrowing S) {
}
