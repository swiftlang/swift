// {"kind":"complete","original":"9815e2ce","signature":"swift::LValueType::get(swift::Type)","signatureAssert":"Assertion failed: (!objectTy->is<LValueType>() && !objectTy->is<InOutType>() && \"cannot have 'inout' or @lvalue wrapped inside an @lvalue\"), function get","signatureNext":"TypeVarBindingProducer::computeNext"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
// REQUIRES: OS=macosx
import Foundation .a
{
  String( #^^#()!)
}
