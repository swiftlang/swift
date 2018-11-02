struct B {
  var other: Int = 0
  lazy var crash: String = {
    return ""
  }()
}

