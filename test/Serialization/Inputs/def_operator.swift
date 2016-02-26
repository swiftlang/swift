prefix operator ~~~ {}
postfix operator ^^ {}

infix operator *- {
  associativity left
  precedence 50
}

infix operator -* {
  associativity right
  precedence 40
}

infix operator *-* {
  associativity none
  precedence 10
  assignment
}

prefix public func ~~~(x: Bool) -> () {}
postfix public func ^^(x: inout Bool) -> () { x = true }
public func *-*(x: Bool, y: Bool) -> () {}
public func  *-(x: inout Bool, y: Bool) -> Bool { x = y; return x }

