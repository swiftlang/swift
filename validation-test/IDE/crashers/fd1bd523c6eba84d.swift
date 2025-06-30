// {"kind":"complete","signature":"swift::Parser::parseLineDirective(bool)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
{
#sourceLocation(file: "", line: 800)
#^COMPLETE^#
