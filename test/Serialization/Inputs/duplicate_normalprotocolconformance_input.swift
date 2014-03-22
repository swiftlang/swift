
protocol A {
  class func a()
}

protocol B : A {
  class func b()
}

struct S {}

extension S : A {
  static func a() { }
}

extension S : B {
  static func b() { }
}
