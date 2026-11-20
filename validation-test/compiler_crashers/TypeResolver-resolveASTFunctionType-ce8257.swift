// {"kind":"typecheck","signature":"(anonymous namespace)::TypeResolver::resolveASTFunctionType(swift::FunctionTypeRepr*, swift::TypeResolutionOptions, (anonymous namespace)::TypeAttrSet*)","signatureAssert":"Assertion failed: (repr->getYieldsTypeRepr()), function resolveASTFunctionType","signatureNext":"TypeResolver::resolveAttributedType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a()
: @yield_once()->b
