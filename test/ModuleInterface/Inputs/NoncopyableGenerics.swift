

// A Swift × Haskell stdlib flavour.

public struct Vector<T: ~Copyable> {

  subscript(_ i: UInt) -> T {
    fatalError("todo")
  }
}




// These are purely to add test coverage of different constructs
public struct Toys {
  static func test_parallelAssignment() {
    var y: Int
    var x: Int
    (x, y) = (10, 11)
  }

}
