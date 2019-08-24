import Resilient
import Foundation
import OneWordSuperclass

public class StaticClass: OneWordSuperclass {
  @objc var first: Int32 = 0
  var middle = GrowsToInt64()
  @objc var last: Int = 0

  @objc public static var offsetOfFirst: Int {
    // IRGen lays out Swift classes that subclass Objective-C classes as if the
    // only superclass was NSObject, so the starting (offset % alignment) isn't
    // always 0. This means that on 32-bit platforms we'll have a gap *before*
    // 'first' when we need 8-byte alignment, rather than after as you'd see in
    // a struct (or base class).
    return max(MemoryLayout<Int>.size, MemoryLayout<GrowsToInt64>.alignment) +
           MemoryLayout<Int>.size
  }

  @objc public static var totalSize: Int {
    return (2 * MemoryLayout<Int>.size) +
           (2 * MemoryLayout<GrowsToInt64>.size) + // alignment
           MemoryLayout<Int>.size
  }
}

/// This class has the same layout as `StaticClass`, but will be accessed using
/// `NSClassFromString` instead of `+class`.
public class DynamicClass: OneWordSuperclass {
  @objc var first: Int32 = 0
  var middle = GrowsToInt64()
  @objc var last: Int = 0

  @objc public static var offsetOfFirst: Int {
    // See above.
    return max(MemoryLayout<Int>.size, MemoryLayout<GrowsToInt64>.alignment) +
           MemoryLayout<Int>.size
  }

  @objc public static var totalSize: Int {
    return (2 * MemoryLayout<Int>.size) +
           (2 * MemoryLayout<GrowsToInt64>.size) + // alignment
           MemoryLayout<Int>.size
  }
}

public class PureSwiftBaseClass {
  var word: Int64 = 0
}

public class PureSwiftClass: PureSwiftBaseClass {
  @objc var first: Int32 = 0
  var middle = GrowsToInt64()
  @objc var last: Int = 0

  @objc public static var offsetOfFirst: Int {
    return (2 * MemoryLayout<Int>.size) + MemoryLayout<Int64>.size
  }

  @objc public static var totalSize: Int {
    return (2 * MemoryLayout<Int>.size) + MemoryLayout<Int64>.size +
           (2 * MemoryLayout<GrowsToInt64>.size) + // alignment
           MemoryLayout<Int>.size
  }
}
