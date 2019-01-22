struct B {
  var other: Int = 0
  lazy var crash: String = {
    return ""
  }()
  lazy var interpolatingMinimal: String = "Hello, \(1)!"
  lazy var interpolatingTorture: String = "Hello, \( "\( "name" )" )\( { "!" }() )"
}

