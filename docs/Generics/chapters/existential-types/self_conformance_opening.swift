import Foundation

@objc protocol Asterozoa {}
class Starfish: Asterozoa {}

func f<T: Asterozoa>(_: T) {
  print(T.self)
}

let x: any Asterozoa = Starfish()
f(x)
