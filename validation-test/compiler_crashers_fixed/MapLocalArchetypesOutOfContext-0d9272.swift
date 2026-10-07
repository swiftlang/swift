// {"kind":"emit-silgen","original":"195c110a","signature":"swift::Type llvm::function_ref<swift::Type (swift::SubstitutableType*)>::callback_fn<swift::MapLocalArchetypesOutOfContext>(long, swift::SubstitutableType*)"}
// RUN: %target-swift-frontend -emit-silgen %s
protocol a {
  func b() -> [() -> Self]
  func x() -> Self
}

do {
  func c1(d: any a) {
    _ = d.b()
  }
  
  func c2(d: any a) {
    _ = d.b
  }

  func c3(d: any a) {
    _ = { d.x() }
  }
  
  func c4(d: any a) {
    _ = {
      _ = d.b()
    }
  }
}
