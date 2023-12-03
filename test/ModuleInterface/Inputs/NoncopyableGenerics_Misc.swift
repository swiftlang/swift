// These are purely to add test coverage of different constructs when emitting modules
public struct _Toys {
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

  public static func pickOne<T>(_ a: T, _ b: T) -> T { return a }
}
