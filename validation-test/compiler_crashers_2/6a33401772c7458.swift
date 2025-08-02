// {"kind":"typecheck","signature":"swift::constraints::FunctionTypeMismatch::FunctionTypeMismatch(swift::constraints::Solution const&, swift::ContextualTypePurpose, swift::Type, swift::Type, llvm::ArrayRef<unsigned int>, swift::constraints::ConstraintLocator*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a < b, c {
  typealias d = b typealias e =
      c class f<b, c> : a<b, c> class g<h, i> : f<(h, i), c> {
    j = d->e class g<h> : f<(h, i), c> {
      k {
        j = d->e
