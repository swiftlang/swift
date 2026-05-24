// {"kind":"emit-silgen","original":"0ca46000","signature":"swift::Type llvm::function_ref<swift::Type (swift::SubstitutableType*)>::callback_fn<swift::MapLocalArchetypesOutOfContext>(long, swift::SubstitutableType*)","signatureNext":"InFlightSubstitution::substType"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
struct a {
  init<b>(_: b) {
  }
}
func c(d: [Any]) {
  d.map { e in
    _openExistential(
      e,
      do: {
        a($0)
      })
  }
}
