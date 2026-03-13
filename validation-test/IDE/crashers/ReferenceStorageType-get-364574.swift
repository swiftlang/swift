// {"kind":"complete","original":"186a3233","signature":"swift::ReferenceStorageType::get(swift::Type, swift::ReferenceOwnership, swift::ASTContext const&)","signatureAssert":"Assertion failed: (!T->hasTypeVariable()), function get"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<b{ c: b weak var d = \ c {
a()#^^#
