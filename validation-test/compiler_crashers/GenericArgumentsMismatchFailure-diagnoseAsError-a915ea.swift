// {"extraArgs":["-language-mode","6"],"kind":"typecheck","original":"cc5ee968","signature":"swift::constraints::GenericArgumentsMismatchFailure::diagnoseAsError()","signatureAssert":"Assertion failed: (purpose != CTP_Unused), function diagnoseAsError","signatureNext":"GenericArgumentsMismatch::diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck -language-mode 6 %s
struct a<b> {
  protocol c {
    associatedtype d
    var g: a<d> { set }
  }
  func e<f: c>() {
    Double {
      let <#pattern#>: WritableKeyPath<f, a> = \.g
    }
  }
}
