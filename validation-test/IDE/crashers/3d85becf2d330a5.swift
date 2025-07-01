// {"kind":"complete","signature":"swift::Parser::consumeIdentifier(swift::Identifier&, bool)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
switch { case borrowing #^COMPLETE^#
