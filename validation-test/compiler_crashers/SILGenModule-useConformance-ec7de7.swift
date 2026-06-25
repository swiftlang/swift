// {"issueID":89520,"kind":"emit-silgen","signature":"swift::Lowering::SILGenModule::useConformance(swift::SILInstruction*, swift::ProtocolConformanceRef)","signatureNext":"Lowering::SILGenModule::useConformancesFromType"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
// https://github.com/swiftlang/swift/issues/89520
struct a<b>: c {
  typealias d = b
}
protocol c where Self == a<d> {
  associatedtype d
}
