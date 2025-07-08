// {"kind":"complete","signature":"swift::SourceManager::findBufferContainingLocInternal(swift::SourceLoc) const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a { lazy b: () = { answer {}#^^#
