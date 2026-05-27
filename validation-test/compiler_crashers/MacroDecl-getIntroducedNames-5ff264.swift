// {"kind":"typecheck","original":"80da2ffb","signature":"swift::MacroDecl::getIntroducedNames(swift::MacroRole, swift::ValueDecl*, llvm::SmallVectorImpl<swift::DeclName>&) const","signatureAssert":"Assertion failed: (!isSpecial() && \"Cannot retrieve identifier from special names\"), function getIdentifier","signatureNext":"MacroIntroducedNameTracker"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@attached(peer suffixed(a)) macro b()
class c {
  @b subscript () -> <#type#>
}
