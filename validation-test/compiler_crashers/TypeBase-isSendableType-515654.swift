// {"kind":"typecheck","original":"7517e5f3","signature":"swift::TypeBase::isSendableType()","signatureNext":"varIsSafeAcrossActors"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b: ~Copyable>() {
  @_nonSendable class c {
    struct e {
      var cache: c
      let d = \cache
    }
  }
}
