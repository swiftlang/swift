// {"kind":"typecheck","original":"f6de5b0a","signature":"swift::IndexSubset::get(swift::ASTContext&, llvm::SmallBitVector const&)","signatureAssert":"Assertion failed: (idx < size()), function operator[]","signatureNext":"AttributeChecker::visitTransposeAttr"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@transpose(of: a) func b()
