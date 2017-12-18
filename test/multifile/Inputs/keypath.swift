final class C<T> {
  var a = 0
  var b = 0

  subscript(x: Int) -> Int { return x }
}

class D<T> {
  var a = 0
  var b = 0

  subscript(x: Int) -> Int { return x }
}

protocol P {
  var a: Int { get set }
  var b: Int { get set }

  subscript(x: Int) -> Int { get set }
}
