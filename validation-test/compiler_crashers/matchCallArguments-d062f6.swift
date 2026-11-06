// {"kind":"typecheck","original":"242b77da","signature":"matchCallArguments(swift::constraints::ConstraintSystem&, swift::FunctionType*, swift::ArgumentList*, llvm::ArrayRef<swift::AnyFunctionType::Param>, llvm::ArrayRef<swift::AnyFunctionType::Param>, swift::constraints::ConstraintKind, swift::constraints::ConstraintLocatorBuilder, std::__1::optional<swift::constraints::TrailingClosureMatching>, llvm::SmallVectorImpl<std::__1::pair<swift::TypeVariableType*, swift::ExistentialArchetypeType*>>&)","signatureNext":"ConstraintSystem::simplifyApplicableFnConstraint"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
class a {
    b<c, g>(
      c ,
      d: (inout c (c) -> g) -> Void = a().b { e, f in
        f(e
