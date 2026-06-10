// {"kind":"typecheck","original":"d61ccce6","signature":"swift::constraints::inference::BindingSet::inferTransitiveKeyPathBindings()","signatureAssert":"Assertion failed: (Kind != AllowedBindingKind::Fallback), function asTransitiveFrom"}
// RUN: not %target-swift-frontend -typecheck %s
enum a {
  case b([a])
  var c {
    switch self {
    case .b(let children):
      children[{
        .map(\d).e ?? 0
      }]
    }
  }
}
