// {"kind":"typecheck","original":"f584f943","signature":"(anonymous namespace)::TypePrinter::printParentType(swift::Type)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  typealias b = Array
  typealias c<d> = a where c<d>.b == <#type#>
}
