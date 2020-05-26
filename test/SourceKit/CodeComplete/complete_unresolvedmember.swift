enum Foo {
  case east, west
  case other(String)
  init(i: Int) {}
  init(s: String) {}
  static var instance: Foo { .east }
  static func create() -> Foo { .west }
}

func test() -> Foo {
  return .
}

// RUN: %sourcekitd-test -req=complete -pos=11:11 %s -- %s > %t.response
// RUN: %diff -u %s.response %t.response

