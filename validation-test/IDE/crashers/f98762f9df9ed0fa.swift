// {"kind":"complete","signature":"swift::rewriting::RewriteContext::getMutableTermForType(swift::CanType, swift::ProtocolDecl const*)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
protocol a { associatedtype b }
protocol c { associatedtype b }
extension c { compose<d where d ==a, d.b == b { #^COMPLETE^#
