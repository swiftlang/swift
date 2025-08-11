// {"kind":"complete","signature":"swift::SourceManager::findBufferContainingLocInternal(swift::SourceLoc) const","signatureAssert":"Assertion failed: (Loc.isValid()), function findBufferContainingLocInternal"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a { lazy b: () = { answer {}#^^#
