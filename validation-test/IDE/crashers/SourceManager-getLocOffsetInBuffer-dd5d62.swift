// {"kind":"complete","original":"05c97bf1","signature":"swift::SourceManager::getLocOffsetInBuffer(swift::SourceLoc, unsigned int) const","signatureAssert":"Assertion failed: (Loc.getPointer() >= Buffer->getBuffer().begin() && Loc.getPointer() <= Buffer->getBuffer().end() && \"Location is not from the specified buffer\"), function getLocOffsetInBuffer","signatureNext":"Lexer::getStateForBeginningOfTokenLoc","useSourceOrderCompletion":true}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-order-completion -source-filename %s
// REQUIRES: OS=macosx
import _Differentiation func a {
#^^#
struct b: Differentiable {
#^c^#
