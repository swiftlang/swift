prefix operator ~~~
postfix operator ^^

infix operator *- : LowPrecedence
precedencegroup LowPrecedence {
  associativity: left
  lowerThan: AssignmentPrecedence
  higherThan: LollipopPrecedence 
}

infix operator -* : LollipopPrecedence
precedencegroup LollipopPrecedence {
  associativity: right
  higherThan: DoubleLollipopPrecedence
}

infix operator *-* : DoubleLollipopPrecedence
precedencegroup DoubleLollipopPrecedence {
  associativity: none
  assignment: true
}

prefix public func ~~~(x: Bool) -> () {}
postfix public func ^^(x: inout Bool) -> () { x = true }
public func *-*(x: Bool, y: Bool) -> () {}
public func  *-(x: inout Bool, y: Bool) -> Bool { x = y; return x }

