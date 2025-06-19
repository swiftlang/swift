// {"signature":"swift::SubstitutionMap::get(swift::GenericSignature, swift::InFlightSubstitution&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < each b { class c protocol d init(e: c & d
