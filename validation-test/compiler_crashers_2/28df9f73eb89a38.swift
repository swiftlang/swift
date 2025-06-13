// {"signature":"lookupReplacedDecl(swift::DeclNameRef, swift::DeclAttribute const*, swift::ValueDecl const*, llvm::SmallVectorImpl<swift::ValueDecl*>&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{                     @_dynamicReplacement(for: )   func a
