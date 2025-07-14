// {"kind":"complete","signature":"swift::TupleTypeElt::TupleTypeElt(swift::Type, swift::Identifier)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
-switch  {
case
< (&a
#^^#
