// RUN: %batch-code-completion

struct S<T: ~Copyable> {}
protocol P: ~Copyable {}

extension S where T: P {
  var bar: Int { 0 }
}
struct R: P {}

// Make sure USR round-tripping works here.
func foo(_ x: S<R>) {
  x.#^COMPLETE^#
  // COMPLETE: Decl[InstanceVar]/CurrNominal:      bar[#Int#]; name=bar
}
