// {"kind":"typecheck","original":"9d13c715","signature":"swift::constraints::ConstraintSystem::diagnoseAmbiguity(llvm::ArrayRef<swift::constraints::Solution>)","signatureAssert":"Assertion failed: (false && \"locator could not be simplified to anchor\"), function diagnoseAmbiguity","signatureNext":"ConstraintSystem::diagnoseAmbiguityWithFixes"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a {
  var
    b : Int
    class c : a {
    var
        b : Int
        @resultBuilder struct d {
          static buildBlock @d func e () {
            \b
          }
    }
  }
}
