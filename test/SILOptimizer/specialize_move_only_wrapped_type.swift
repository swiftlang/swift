// RUN: %target-swift-frontend -emit-sil -O %s

public struct Mutex<T> {
  public init(_: T) {}
}

public struct Locked<T> {
  public let mutex: Mutex<T>

  public init(_ rawValue: consuming T) {
    mutex = Mutex(rawValue)
  }
}

_ = Locked { }
