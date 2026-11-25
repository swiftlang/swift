// {"kind":"typecheck","signature":"swift::constraints::getConversionBehavior(swift::Type)","signatureNext":"hasProperSubtypes"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// rdar://193164508
let x: any Collection<3> = [1]
