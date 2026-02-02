// {"kind":"typecheck","signature":"checkRequirementsImpl(llvm::ArrayRef<swift::Requirement>, bool)","signatureAssert":"Assertion failed: (!firstType->hasTypeVariable()), function checkRequirementsImpl"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b: ~Copyable
  extension a: Copyable where b: Copyable
    let c  0 as a
...
