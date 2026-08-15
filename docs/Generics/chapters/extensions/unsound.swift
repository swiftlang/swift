protocol Bar {
  associatedtype Beer
  func brew() -> Beer
}

protocol Pub {
  associatedtype Beer
  func pour() -> Beer
}

struct BrewPub<T> {}

extension BrewPub: Bar where T: Equatable {
  typealias Beer = String
  func brew() -> String { return "" }
}

extension BrewPub: Pub where T: Numeric {
  typealias Beer = Float
  func pour() -> Float { return 1.0 }
}

func both<T: Bar & Pub>(_ t: T) -> (T.Beer, T.Beer) {
  return (t.brew(), t.pour())
}

let result = both(BrewPub<Int>())
print(result)
