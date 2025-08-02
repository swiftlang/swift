// {"kind":"typecheck","original":"2e6c789c","signature":"swift::MacroDecl::getIntroducedNames(swift::MacroRole, swift::ValueDecl*, llvm::SmallVectorImpl<swift::DeclName>&) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a
@TaskLocal init()
