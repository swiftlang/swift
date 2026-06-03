// {"kind":"typecheck","original":"e95f74e2","signature":"swift::constraints::MissingArgumentsFailure::forFixIt(llvm::raw_svector_ostream&, swift::AnyFunctionType::Param const&) const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"MissingArgumentsFailure::diagnoseSingleMissingArgument"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
    b($c: @autoclosure () -> Void)
  {
    b(
