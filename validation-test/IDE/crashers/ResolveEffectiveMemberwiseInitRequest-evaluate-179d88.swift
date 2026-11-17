// {"kind":"complete","original":"00e99848","signature":"swift::ResolveEffectiveMemberwiseInitRequest::evaluate(swift::Evaluator&, swift::NominalTypeDecl*) const","signatureAssert":"Assertion failed: (!memberwiseInitDecl && \"Memberwise initializer already found\"), function evaluate","signatureNext":"ResolveEffectiveMemberwiseInitRequest::OutputType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
// REQUIRES: OS=macosx
// REQUIRES: differentiable_programming
import _Differentiation
struct a: AdditiveArithmetic {
  #^^#  init(b: Float)
  b: Float  init(b: Float
