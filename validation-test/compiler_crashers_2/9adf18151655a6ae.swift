// {"signature":"isVanishingTupleConformance(swift::NormalProtocolConformance*, swift::SubstitutionMap)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
typealias a<b> = (repeat b)protocol c extension a : c
