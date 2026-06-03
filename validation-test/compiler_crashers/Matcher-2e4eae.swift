// {"kind":"typecheck","original":"b42f1423","signature":"swift::CanTypeVisitor<swift::TypeMatcher<desugarSameTypeRequirement(swift::Requirement, swift::SourceLoc, llvm::SmallVectorImpl<swift::Requirement>&, llvm::SmallVectorImpl<swift::InverseRequirement>&, llvm::SmallVectorImpl<swift::rewriting::RequirementError>&)::Matcher>::MatchVisitor, bool, swift::Type, swift::Type>::visit(swift::CanType, swift::Type, swift::Type)","signatureNext":"desugarRequirement","useGuardMalloc":true}
// RUN: env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
// REQUIRES: no_asan
// REQUIRES: target-same-as-host
struct a < b
  struct c {
    typealias d = a
    func e < f where f.d ==, f == c
