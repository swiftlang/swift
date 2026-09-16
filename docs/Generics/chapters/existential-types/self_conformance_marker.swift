protocol Heffalump: Sendable {
  init()  // okay, no soundness hole!
}
struct G<T: Sendable> {}
let g = G<any Heffalump>()  // okay
