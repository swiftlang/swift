// RUN: %target-swift-frontend -Osize -wmo -sil-verify-all -emit-sil %s -o /dev/null

public var storage = false

@inline(never)
public func opaque(_ value: Bool) -> Bool {
  storage = value
  return storage
}

open class Repro {
  public init() {}

  open class var value: Int { 0 }

  public func consume(_ value: Int) {}

  public func run(_ condition: Bool, _ other: Bool) -> Bool {
    Self.helper(
      condition: condition,
      first: {
        var shouldConsume = true
        for _ in 0..<20 {
          shouldConsume = opaque(shouldConsume)
        }
        if shouldConsume {
          consume(Self.value)
        }
      },
      second: {
        var value = other
        for _ in 0..<20 {
          value = opaque(value)
        }
        _ = opaque(value)
      }
    )
  }

  public static func helper(
    condition: Bool,
    first: () -> Void,
    second: () -> Void
  ) -> Bool {
    var shouldRun = condition
    for _ in 0..<20 {
      shouldRun = opaque(shouldRun)
    }
    if shouldRun {
      first()
    } else {
      second()
    }
    return shouldRun
  }
}
