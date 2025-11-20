// {"kind":"complete","signature":"swift::MetatypeType::get(swift::Type, std::__1::optional<swift::MetatypeRepresentation>, swift::ASTContext const&)"}
// RUN: %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
[ .
#^COMPLETE^#,
( repeat.a ), (3, .b
