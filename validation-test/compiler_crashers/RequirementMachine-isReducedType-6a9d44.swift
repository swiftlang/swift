// {"kind":"typecheck","original":"51beded5","signature":"swift::rewriting::RequirementMachine::isReducedType(swift::Type) const::Walker::walkToTypePre(swift::Type)","signatureNext":"Traversal::doIt"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  protocol c where Self: a<> {
    associatedtype b
  }
}
