// {"kind":"typecheck","original":"e95f74e2","signature":"swift::constraints::MissingArgumentsFailure::forFixIt(llvm::raw_svector_ostream&, swift::AnyFunctionType::Param const&) const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"MissingArgumentsFailure::diagnoseSingleMissingArgument"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
    b($c: @autoclosure () -> Void)
  {
    b(
