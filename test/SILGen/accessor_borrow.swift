// RUN: %target-swift-emit-silgen -module-name accessor_borrow -Xllvm -sil-full-demangle %s | %FileCheck %s

struct NE: ~Escapable {}

struct NEContainer: ~Copyable {
  // This accessor borrows self. Do not synthesize a getter.
  var ne_coroutine: NE {
    _read {
      NE()
    }
  }
}
