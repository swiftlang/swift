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

@prefix func ~~~(x: Bool) -> () {}
@postfix @assignment func ^^(inout x: Bool) -> () { x = true }
@infix func *-*(x: Bool, y: Bool) -> () {}
@infix @assignment func  *-(inout x: Bool, y: Bool) -> Bool { x = y; return x }

