// {"kind":"typecheck","original":"232744a0","signature":"swift::FunctionType::get(llvm::ArrayRef<swift::AnyFunctionType::Param>, swift::Type, std::__1::optional<swift::ASTExtInfo>)","signatureAssert":"Assertion failed: (isConsistentAboutIsolation(info, params)), function FunctionType","signatureNext":"ConstraintSystem::simplifyKeyPathConstraint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
actor a
  struct d: ExpressibleByStringInterpolation {
    stringInterpolation: b
    struct b: StringInterpolationProtocol {
        appendLiteral( String)
        appendInterpolation(
          c: (isolated a) -> String
        let : d = "\(e: \.f)"
