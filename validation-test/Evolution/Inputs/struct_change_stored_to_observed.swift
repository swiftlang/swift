
#if BEFORE

public func getVersion() -> Int {
  return 0
}

public struct ChangeStoredToObserved {
  public var friends: [String] = []
  public var count: Int = 0

  public var friend: String = "cat"

  public init() {}
}

#else

public func getVersion() -> Int {
  return 1

}

public struct ChangeStoredToObserved {
  public var friends: [String] = []
  public var count: Int = 0

  public var friend: String = "cat" {
    willSet {
      friends.append(friend)
    }
    didSet {
      count += 1
    }
  }

  public init() {}
}

#endif
