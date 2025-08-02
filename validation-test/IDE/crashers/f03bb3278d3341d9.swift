// {"kind":"complete","signature":"swift::Parser::consumeTopLevelDecl(swift::ParserPosition, swift::TopLevelCodeDecl*)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
guard {
#if #^COMPLETE^#
0
