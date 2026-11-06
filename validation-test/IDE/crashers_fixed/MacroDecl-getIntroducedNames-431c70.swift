// {"kind":"complete","original":"610a8a6d","signature":"swift::MacroDecl::getIntroducedNames(swift::MacroRole, swift::ValueDecl*, llvm::SmallVectorImpl<swift::DeclName>&) const","signatureAssert":"Assertion failed: (!isSpecial() && \"Cannot retrieve identifier from special names\"), function getIdentifier","signatureNext":"SourceLookupCache::populateAuxiliaryDeclCache"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@TaskLocal init()
}
#^^#
