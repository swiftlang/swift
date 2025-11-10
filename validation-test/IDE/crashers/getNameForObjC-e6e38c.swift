// {"kind":"complete","original":"afd4a708","signature":"swift::objc_translation::getNameForObjC(swift::ValueDecl const*, swift::objc_translation::CustomNamesOnly_t)","signatureAssert":"Assertion failed: (name->getNumSelectorPieces() == 1), function getNameForObjC"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
// REQUIRES: objc_interop
@objc(:a @ protocol b
.#^^#
