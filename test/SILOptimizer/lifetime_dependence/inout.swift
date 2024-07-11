// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature NonescapableTypes

// REQUIRES: asserts
// REQUIRES: swift_in_compiler

struct Span<T>: ~Escapable {
  private var base: UnsafePointer<T>
  private var count: Int

  @_unsafeNonescapableResult
  init(base: UnsafePointer<T>, count: Int) {
    self.base = base
    self.count = count
  }

  init<S>(base: UnsafePointer<T>, count: Int, generic: borrowing S) -> dependsOn(generic) Self {
    self.base = base
    self.count = count
  }
}

extension Array {
  // TODO: comment out dependsOn(scoped)
  borrowing func span() -> /* dependsOn(scoped self) */ Span<Element> {
    /* not the real implementation */
    let p = self.withUnsafeBufferPointer { $0.baseAddress! }
    return Span(base: p, count: 1)
  }
}

// Reassign an inout argument to a value that depends on the lifetime of another argument.
func mayReassign(span: dependsOn(a) inout Span<Int>, to a: Array<Int>) {
  span = a.span()
}
