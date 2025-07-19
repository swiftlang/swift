// {"signature":"swift::constraints::ContextualFailure::diagnoseConversionToBool() const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  wrappedValue : Bool {
    @a var b : Int
