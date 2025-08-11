// {"signature":"swift::ClassDecl::isSuperclassOf(swift::ClassDecl const*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a {
  typealias b = <#type#>
}
class c< d > : a< > {
}
c.b
