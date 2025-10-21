// {"kind":"complete","original":"afd4a708","signature":"swift::ClangUSRGenerationRequest::evaluate(swift::Evaluator&, swift::ValueDecl const*) const","signatureAssert":"Assertion failed: (ObjCName.second), function printObjCUSR"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
// REQUIRES: objc_interop
@objc(: @ protocol a
.#^^#
