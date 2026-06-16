// {"kind":"typecheck","signature":"swift::getParameterAt(swift::ConcreteDeclRef, unsigned int)"}
// RUN: not %target-swift-frontend -typecheck %s
{
  func a()
  @<#type#> #b(a(c))
}
