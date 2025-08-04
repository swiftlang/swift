// {"kind":"complete","signature":"swift::FragileFunctionKindRequest::evaluate(swift::Evaluator&, swift::DeclContext*) const","signatureAssert":"Assertion failed: (VD->hasParameterList()), function evaluate"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
enum a {
  case ( = {
      enum b :
        #^COMPLETE^#
