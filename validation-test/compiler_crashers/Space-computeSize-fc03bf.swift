// {"kind":"typecheck","original":"09e735b9","signature":"(anonymous namespace)::SpaceEngine::Space::computeSize(swift::DeclContext const*, llvm::SmallPtrSetImpl<swift::TypeBase*>&) const","stackOverflow":true}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a<b> {
  case (a<c<b>>)
  d{switch self{}}
  struct c<b> {
  }
}
