// {"kind":"typecheck","original":"e51afbca","signature":"diagnoseContextualFunctionCallGenericAmbiguity(swift::constraints::ConstraintSystem&, llvm::ArrayRef<std::__1::pair<swift::constraints::Solution const*, swift::constraints::ConstraintFix const*>>, llvm::ArrayRef<std::__1::pair<swift::constraints::Solution const*, swift::constraints::ConstraintFix const*>>)","signatureAssert":"Assertion failed: (!empty()), function front"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a throw withThrowingTaskGroup(of : a) {
b in
