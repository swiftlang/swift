import FrameworkA

final class SubThing: BaseThing {
  var y = 2
  var z = 3

  override init() {}

  func printThis() {
    print("x \(x)")
    print("y \(y)")
    print("z \(z)")
  }
}
