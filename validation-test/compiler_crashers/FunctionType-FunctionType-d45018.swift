// {"kind":"typecheck","original":"232744a0","signature":"swift::FunctionType::FunctionType(llvm::ArrayRef<swift::AnyFunctionType::Param>, llvm::ArrayRef<swift::AnyFunctionType::Yield>, swift::Type, std::__1::optional<swift::ASTExtInfo>, swift::ASTContext const*, swift::RecursiveTypeProperties)","signatureAssert":"Assertion failed: (isConsistentAboutIsolation(info, params)), function FunctionType","signatureNext":"FunctionType::get"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
actor a
  struct d: ExpressibleByStringInterpolation {
    stringInterpolation: b
    struct b: StringInterpolationProtocol {
        appendLiteral( String)
        appendInterpolation(
          c: (isolated a) -> String
        let : d = "\(e: \.f)"
