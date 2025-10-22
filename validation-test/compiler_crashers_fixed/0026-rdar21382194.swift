// RUN: not %target-swift-frontend %s -typecheck

/// Abstraction of numeric types that approximate real numbers
protocol ApproximateReal {
  init() // zero
  func * (Self,Self) -> Self
  func + (Self,Self) -> Self
  func / (Self,Self) -> Self
  func - (Self,Self) -> Self
  prefix func + (Self) -> Self
  prefix func - (Self) -> Self
}

extension Double : ApproximateReal {}
extension Float : ApproximateReal {}

// Abstraction of a mathematical vector
protocol Vector {
  init()
  associatedtype Element : ApproximateReal
  func dotProduct(Self) -> Element


  // Extras
  var count: Int {get}
  subscript(Int) -> Element {get set}
  associatedtype Tail
}

struct EmptyVector<T: ApproximateReal> : Vector {
  init() {}
  typealias Element = T
  func dotProduct(other: EmptyVector) -> Element {
    return Element() // zero
  }
  var count: Int { return 0 }
  
  subscript(i: Int) -> Element {
    get { fatalError("subscript out-of-range") }
    set { fatalError("subscript out-of-range") }
  }
}

struct Vector<Tail: Vector> : Vector {
  typealias Element = Tail.Element
  
  init(head: Element, tail: Tail) {
    self.head = head
    self.tail = tail
  }

  init() {
    self.head = Element()
    self.tail = Tail()
  }

  /*
  init(scalar: Element) {
    self.head = Element()
    self.tail = Tail()
  }
  */
  
  func dotProduct(other: Vector) -> Element {
    return head * other.head + tail.dotProduct(other.tail)
  }

  var count: Int { return tail.count + 1 }
  var head: Element
  var tail: Tail

  subscript(i: Int) -> Element {
    get { return i == 0 ? head : tail[i - 1] }
    set { if i == 0 { head = newValue } else { tail[i - 1] = newValue } }
  }
}

extension Vector where Tail == EmptyVector<Element> {
  init(_ scalar: Element) {
    self.init()
    self[0] = scalar
  }
}

//===--- A nice operator for composing vectors ----------------------------===//
//===--- there's probably a more appropriate symbol -----------------------===//
infix operator ⋮ {
  associativity right
  precedence 1
}

func ⋮ <T: ApproximateReal> (lhs: T, rhs: T) -> Vector<Vector<EmptyVector<T> > > {
  return Vector(head: lhs, tail: Vector(head: rhs, tail: EmptyVector()))
}
func ⋮ <T: ApproximateReal, U where U.Element == T> (lhs: T, rhs: Vector<U>) -> Vector<Vector<U> > {
  return Vector(head: lhs, tail: rhs)
}

extension Vector : CustomDebugStringConvertible {
  var debugDescription: String {
    if count == 1 {
      return "Vector(\(String(reflecting: head)))"
    }
    return "\(String(reflecting: head)) ⋮ " + (count == 2 ? String(reflecting: self[1]) : String(reflecting: tail))
  }
}

//===--- Test -------------------------------------------------------------===//
print(Vector(head: 3.0, tail: EmptyVector()))
print(3.0 ⋮ 4.0 ⋮ 5.0)
print( (3.0 ⋮ 4.0 ⋮ 5.0).dotProduct(6.0 ⋮ 7.0 ⋮ 8.0) ) // 86.0

// print( (3.0 ⋮ 4.0 ⋮ 5.0).dotProduct(6.0 ⋮ 7.0) ) // Won't compile

