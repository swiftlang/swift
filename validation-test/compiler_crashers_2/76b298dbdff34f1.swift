// {"signature":"swift::TypeBase::getContextSubstitutions(swift::DeclContext const*, swift::GenericEnvironment*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a < b { wrappedValue : c var projectedValue init(projectedValue d) func e(@a & f g : b
