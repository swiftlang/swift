// {"kind":"complete","signature":"swift::DeclContext::isInnermostContextGeneric() const"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
case ( <#expression#>= { enum a : #^COMPLETE^#
