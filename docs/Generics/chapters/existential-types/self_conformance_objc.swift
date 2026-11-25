@objc protocol Woozle {
  func stealHoney()  // note: no inits or static methods!
}
struct G<T: Woozle> {}
let x = G<any Woozle>()             // okay
let y = G<any Woozle & Sequence>()  // not allowed!
