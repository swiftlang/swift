// {"kind":"typecheck","original":"485bd562","signature":"swift::EnumRawValuesRequest::evaluate(swift::Evaluator&, swift::EnumDecl*) const","signatureNext":"EnumRawValuesRequest::OutputType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  ~
  enum c<d: a>: d.b where d.b: StringProtocol & ExpressibleByNilLiteral {
    case  = nil
  }
}
