// Compiled once per client image, with -DCLIENT_A or -DCLIENT_B.
import MyKit
import Common

public struct Client {
  public static func boxInt() -> Any.Type { Box<Int>.self }
  public static func either() -> Any.Type { Either<String, Box<Int>>.self }
  public static func point() -> Any.Type { Point.self }
  public static func needsBoxInt() -> Any.Type { needsType(Box<Int>.self) }
  public static func needsPoint() -> Any.Type { needsType(Point.self) }
  public static func makeBox() -> Any { Box(42) }
  public static func makePoint() -> Any { Point(x: 1, y: 2) }
  public static func inner() -> Any.Type { Outer.Inner.self }
  public static func nested() -> Any.Type { Box<Int>.Nested<String>.self }
  public static func makeInner() -> Any { Outer.Inner(v: 7) }

  // Witnesses for generic requirements.
  public static func describePoint() -> [String] { describe(Point(x: 1, y: 2)) }
  public static func describeBox() -> [String] { describe(Box(5)) }
  public static func describeEither() -> [String] {
    describe(Either<Int, Int>.right(7))
  }
  public static func countPoint() -> Int {
    countMatching(Point(x: 1, y: 2), [1, 2, 3, 2])
  }
  public static func makePointFromArray() -> Point {
    make(Point.self, from: [3, 4])
  }
  public static func makeBoxFromArray() -> Box<String> {
    make(Box<String>.self, from: ["a"])
  }
  public static func needsTransformerPoint() -> Any.Type {
    needsTransformer(Point.self)
  }
  public static func labelPoint() -> String {
    labelGenerically(Point(x: 1, y: 2))
  }
  public static func needsTransformerBox() -> Any.Type {
    needsTransformer(Box<Int>.self)
  }
}
