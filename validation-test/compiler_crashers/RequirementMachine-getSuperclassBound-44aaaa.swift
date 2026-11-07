// {"kind":"typecheck","original":"08eea735","signature":"swift::rewriting::RequirementMachine::getSuperclassBound(swift::Type, llvm::ArrayRef<swift::GenericTypeParamType*>) const","signatureNext":"checkTypeWitness"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a: b where c == Self
  protocol b {
    struct d: b
      typealias d
        struct e        ;
        #f
      {        e
