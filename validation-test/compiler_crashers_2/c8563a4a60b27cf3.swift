// {"signature":"swift::CanTypeVisitor<swift::TypeMatcher<desugarSameTypeRequirement(swift::Requirement, swift::SourceLoc, llvm::SmallVectorImpl<swift::Requirement>&, llvm::SmallVectorImpl<swift::InverseRequirement>&, llvm::SmallVectorImpl<swift::rewriting::RequirementError>&)::Matcher>::MatchVisitor, bool, swift::Type, swift::Type>::visit(swift::CanType, swift::Type, swift::Type)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b associatedtype c associatedtype d func e(b, c) -> d
}
struct f < j, g : a where h == g.c struct i<j> : a {
  e(j, _) extension f where g == i<j>
