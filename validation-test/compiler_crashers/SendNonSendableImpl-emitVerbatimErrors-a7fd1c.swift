// {"extraArgs":["-language-mode","6"],"kind":"emit-sil","original":"769421bc","signature":"(anonymous namespace)::SendNonSendableImpl::emitVerbatimErrors()","signatureNext":"SendNonSendable::run"}
// RUN: not --crash %target-swift-frontend -emit-sil -language-mode 6 %s

// FIXME: With the once functions adopted in the stdlib, the body of
// withTaskGroup is a once closure and this doesn't crash, but the 'sending'
// error has no location.
// XFAIL: *
actor a {
  func b(c: () -> Int) async {
    await withTaskGroup {
      d in
      d.addTask {
        withoutActuallyEscaping(c) { e in
        }
      }
    }
  }
}
