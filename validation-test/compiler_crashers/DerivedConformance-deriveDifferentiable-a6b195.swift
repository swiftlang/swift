// {"kind":"emit-sil","signature":"swift::DerivedConformance::deriveDifferentiable(swift::AssociatedTypeDecl*)","signatureNext":"AssociatedTypeInference::solve"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
// REQUIRES: OS=macosx
// REQUIRES: differentiable_programming
import _Differentiation
@propertyWrapper struct a {
  var wrappedValue: <#type#> {
    struct b: Differentiable {
      @a var c: <#type#>
    }
  }
}
