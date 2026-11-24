// {"kind":"complete","original":"a4186e31","signature":"swift::FragileFunctionKindRequest::OutputType swift::Evaluator::getResultUncached<swift::FragileFunctionKindRequest, swift::FragileFunctionKindRequest::OutputType swift::evaluateOrDefault<swift::FragileFunctionKindRequest>(swift::Evaluator&, swift::FragileFunctionKindRequest, swift::FragileFunctionKindRequest::OutputType)::'lambda'()>(swift::FragileFunctionKindRequest const&, swift::FragileFunctionKindRequest::OutputType swift::evaluateOrDefault<swift::FragileFunctionKindRequest>(swift::Evaluator&, swift::FragileFunctionKindRequest, swift::FragileFunctionKindRequest::OutputType)::'lambda'())::'lambda'()::operator()() const","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit","signatureNext":"DeclContext::getFragileFunctionKind"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
macro a(
    = {
      macro b =
        #^^#
    }
) ->
