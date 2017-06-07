enum NoRawTypeKey {
  case a, b, c
}

enum StringKey : String {
  case a = "A", b, c = "Foo"
}

enum IntKey : Int {
  case a = 3, b, c = 1
}
