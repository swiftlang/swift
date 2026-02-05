// {"kind":"emit-silgen","original":"195c110a","signature":"swift::Type llvm::function_ref<swift::Type (swift::SubstitutableType*)>::callback_fn<swift::MapLocalArchetypesOutOfContext>(long, swift::SubstitutableType*)"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a {
  func b() -> [() -> Self]
}
do {
  func c(d: a) {
    let e = d.b
  }
}
