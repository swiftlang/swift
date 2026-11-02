// {"kind":"emit-ir","signature":"(anonymous namespace)::FragileWitnessTableBuilder::addMethod(swift::SILDeclRef)","signatureAssert":"Assertion failed: (!empty()), function front","signatureNext":"SILWitnessVisitor::visitFuncDecl"}
// RUN: not --crash %target-swift-frontend -emit-ir -module-name Test -cxx-interoperability-mode=default -I %S/Inputs/FragileWitnessTableBuilder-addMethod-364751 %s

import ConformsTo
protocol P {
  func f()
}
func take(_ x: some P) {}
take(S())
