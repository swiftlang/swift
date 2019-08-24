public struct Test1 {
  lazy var property: String = "help"
}

public class Test2 {
  var x = 0
  var y = 1

  lazy var property = {
    return [self.x, self.y]
  }()
}
