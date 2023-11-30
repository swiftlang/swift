

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

  public struct rdar118697289_S1<Element> {
    let element: Element
    func f() -> Element { element }
  }

  public struct rdar118697289_S2<Element> {
      let element: Element
      subscript(i: Int) -> Element {
          element
      }
  }

}
