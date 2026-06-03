// {"kind":"complete","original":"a4186e31","signature":"swift::FragileFunctionKindRequest::evaluate(swift::Evaluator&, swift::DeclContext*) const","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit","signatureNext":"FragileFunctionKindRequest::OutputType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
macro a(
    = {
      macro b =
        #^^#
    }
) ->
