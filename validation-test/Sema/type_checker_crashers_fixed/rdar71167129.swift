// RUN: not %target-swift-frontend %s -typecheck

final class Whatever {
  static var instance: Self!

  static var shared: Self {
    return instance ?? {
      instance = Self()
      return instance
    }
  }
}
