operator prefix ~~~ {}
operator postfix ^^ {}

operator infix *- {
  associativity left
  precedence 50
}

operator infix -* {
  associativity right
  precedence 40
}

operator infix *-* {
  associativity none
  precedence 10
}

prefix public func ~~~(x: Bool) -> () {}
@assignment postfix public func ^^(inout x: Bool) -> () { x = true }
public func *-*(x: Bool, y: Bool) -> () {}
@assignment public func  *-(inout x: Bool, y: Bool) -> Bool { x = y; return x }

