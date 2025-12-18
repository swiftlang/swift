// RUN: %target-swift-frontend -c -O %s -target %target-swift-6.0-abi-triple

// REQUIRES: synchronization

import Synchronization

class Locked<T> {
  let mutex: Mutex<T>

  init(_ rawValue: T) {
    mutex = Mutex(rawValue)
  }
}

class Foo {}

_ = Locked(Foo())
