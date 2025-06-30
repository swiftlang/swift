// {"kind":"typecheck","signature":"swift::AnyFunctionType* swift::TypeBase::castTo<swift::AnyFunctionType>()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
subscript(a: Int?) -> <#type#> {
  var b = a
  Float(b)
}
