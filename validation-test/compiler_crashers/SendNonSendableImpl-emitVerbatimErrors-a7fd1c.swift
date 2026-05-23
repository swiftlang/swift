// {"extraArgs":["-language-mode","6"],"kind":"emit-sil","original":"769421bc","signature":"(anonymous namespace)::SendNonSendableImpl::emitVerbatimErrors()","signatureNext":"SendNonSendable::run"}
// RUN: not --crash %target-swift-frontend -emit-sil -language-mode 6 %s
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
