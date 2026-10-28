// {"kind":"typecheck","original":"aebadcbe","signature":"swift::constraints::inference::BindingSet::operator<(swift::constraints::inference::BindingSet const&)","signatureAssert":"Assertion failed: (numDefaultable >= unviable), function getNumViableDefaultableBindings"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b, c>(d: b, e: c) -> f {
  [d, e].flatMap {
    $0
  }
}
