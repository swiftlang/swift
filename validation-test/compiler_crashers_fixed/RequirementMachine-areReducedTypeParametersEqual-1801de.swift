// {"kind":"typecheck","signature":"swift::rewriting::RequirementMachine::areReducedTypeParametersEqual(swift::Type, swift::Type) const"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a {
  typealias Index extension Collection where Self : a{b : Index} protocol a
