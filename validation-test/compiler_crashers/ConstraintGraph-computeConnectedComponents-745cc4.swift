// {"kind":"typecheck","original":"281cf839","signature":"swift::constraints::ConstraintGraph::computeConnectedComponents(llvm::ArrayRef<swift::TypeVariableType*>)","signatureAssert":"Assertion failed: (component != components.end()), function operator()","signatureNext":"SplitterStep::computeFollowupSteps"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  func b(c: Bool) -> (some a some a) {
    (if c {
      return (d
      e
