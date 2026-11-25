protocol Heffalump: Sendable {
  init()  // okay, no soundness hole!
}
struct G<T: Sendable> {}
let x = G<any Heffalump>()            // okay
let y = G<any Sequence & Sendable>()  // also okay
