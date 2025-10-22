// {"kind":"typecheck","signature":"swift::AbstractStorageDecl::getValueInterfaceType() const","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a {
}
{
  lazy var b = {
    c
  }
  var c = a
}
