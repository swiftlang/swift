// {"kind":"complete","original":"7bac7625","signature":"swift::constraints::ConstraintSystem::compareSolutions(swift::constraints::ConstraintSystem&, llvm::ArrayRef<swift::constraints::Solution>, swift::constraints::SolutionDiff const&, unsigned int, unsigned int)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ConstraintSystem::findBestSolution"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a {
  var b: (String, c: [a]) {
    class d: a {
      var b: (Int, String, c: String)
      >
      let <#pattern#>: ReferenceWritableKeyPath<d, a> = \.b.c[#^^#]
    }
  }
}
