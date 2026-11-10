// {"kind":"typecheck","signature":"swift::TypeBase::getContextSubstitutions(swift::DeclContext const*, swift::GenericEnvironment*)","signatureAssert":"Assertion failed: (!baseTy->is<LValueType>() && !baseTy->is<AnyMetatypeType>() && !baseTy->is<ErrorType>()), function getContextSubstitutions","signatureNext":"TypeBase::getContextSubstitutionMap"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a < b { wrappedValue : c var projectedValue init(projectedValue d) func e(@a & f g : b
