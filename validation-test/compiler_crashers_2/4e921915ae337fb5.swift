// {"signature":"swift::NormalProtocolConformance::setTypeWitness(swift::AssociatedTypeDecl*, swift::Type, swift::TypeDecl*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a typealias b<c> = () extension b : a
