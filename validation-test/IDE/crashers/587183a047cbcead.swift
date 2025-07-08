// {"kind":"complete","signature":"swift::FragileFunctionKindRequest::evaluate(swift::Evaluator&, swift::DeclContext*) const"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
enum a {
  case ( = {
      enum b :
        #^COMPLETE^#
