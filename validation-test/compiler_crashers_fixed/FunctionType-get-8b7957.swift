// {"kind":"typecheck","original":"cc0c34b4","signature":"swift::FunctionType::get(llvm::ArrayRef<swift::AnyFunctionType::Param>, swift::Type, std::__1::optional<swift::ASTExtInfo>)","signatureAssert":"Assertion failed: (isConsistentAboutIsolation(info, params)), function FunctionType","signatureNext":"ConstraintSystem::getEffectiveOverloadType"}
// RUN: not %target-swift-frontend -typecheck %s
struct a {
  subscript(isolated  Actor)  String {
    a[
