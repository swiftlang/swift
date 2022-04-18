open class Base {
   var x = 1
}
internal struct TypeContainer {

  enum SomeEnum:String {
    case FirstCase = "first"
    case SecondCase = "second"
  }
}

internal class SubClass : Base {
    var y : TypeContainer.SomeEnum

    override init() {
        y = .FirstCase
    }
}
