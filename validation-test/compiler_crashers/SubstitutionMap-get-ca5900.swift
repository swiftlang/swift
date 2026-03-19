// {"kind":"typecheck","signature":"swift::SubstitutionMap::get(swift::GenericSignature, swift::InFlightSubstitution&)","signatureAssert":"Assertion failed: ((replacement->hasError() || gp->isParameterPack() == replacement->is<PackType>()) && \"replacement for pack parameter must be a pack type\"), function get","signatureNext":"ExistentialTypeGeneralization::get"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < each b { class c protocol d init(e: c & d
