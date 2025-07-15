// {"kind":"typecheck","signature":"swift::AbstractStorageDecl::getValueInterfaceType() const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a {
}
{
  lazy var b = {
    c
  }
  var c = a
}
