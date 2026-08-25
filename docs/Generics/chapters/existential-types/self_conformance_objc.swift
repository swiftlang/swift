@objc protocol Woozle {
  func stealHoney()  // but no inits or static methods
}
struct G<T: Woozle> {}
let g = G<any Woozle>()  // okay
