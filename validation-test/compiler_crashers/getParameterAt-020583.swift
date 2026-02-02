// {"kind":"typecheck","original":"b56d8c69","signature":"swift::getParameterAt(swift::ConcreteDeclRef, unsigned int)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  func a()
  @<#type#> #b(a(c))
}
