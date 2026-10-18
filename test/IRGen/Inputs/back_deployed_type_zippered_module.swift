// Back-deployed only on the target variant's platform.
@backDeployed(before: iOS 99)
@frozen public struct VariantOnly {
  public var x: Int
  public init(x: Int) { self.x = x }
}

// Back-deployed only on the primary platform.
@backDeployed(before: macOS 99)
@frozen public struct PrimaryOnly {
  public var x: Int
  public init(x: Int) { self.x = x }
}

// The primary deployment target guarantees this one, but the variant's
// doesn't.
@backDeployed(before: macOS 11, iOS 99)
@frozen public struct NeededForVariant {
  public var x: Int
  public init(x: Int) { self.x = x }
}

// Both deployment targets guarantee this one.
@backDeployed(before: macOS 11, iOS 14)
@frozen public struct NotNeeded {
  public var x: Int
  public init(x: Int) { self.x = x }
}

@backDeployed(before: macOS 99, iOS 99)
@frozen public struct Both {
  public var x: Int

  // Only opted out on macOS, so still back-deployed for the variant.
  @available(macOS 99, *)
  public func macOnly() -> Int { x }

  // Opted out on both platforms.
  @available(macOS 99, iOS 99, *)
  public func both() -> Int { x }
}
