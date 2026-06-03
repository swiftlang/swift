// {"kind":"typecheck","original":"09e735b9","signature":"(anonymous namespace)::SpaceEngine::Space::computeSize(swift::DeclContext const*, llvm::SmallPtrSetImpl<swift::TypeBase*>&) const","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a<b> {
  case (a<c<b>>)
  d{switch self{}}
  struct c<b> {
  }
}
