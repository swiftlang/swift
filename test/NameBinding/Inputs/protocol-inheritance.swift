public protocol Critter {
  associatedtype Fur
}
public protocol Pet {}

public typealias Cat = Critter & Pet

public protocol Kitten : Cat {}

extension Kitten {
  public func pet() -> Fur {
    while true {}
  }
}

public final class Meow<Purrs> : Kitten {
  public typealias Fur = Purrs
}
