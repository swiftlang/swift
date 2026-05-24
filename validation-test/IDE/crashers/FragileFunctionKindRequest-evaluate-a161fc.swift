// {"kind":"complete","original":"cd594609","signature":"swift::FragileFunctionKindRequest::evaluate(swift::Evaluator&, swift::DeclContext*) const","signatureAssert":"Assertion failed: (VD->hasParameterList()), function evaluate","signatureNext":"FragileFunctionKindRequest::OutputType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
enum a {
  case (
    <#type#> = {
      func b() {
        switch <#expression#> {
        case #^^#:
          let c
        }
      }
    })
}
