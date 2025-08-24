// {"kind":"emit-silgen","signature":"swift::Lowering::SILGenModule::useConformance(swift::ProtocolConformanceRef)"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a {
}
struct b<c: a> {
}
extension [b<Int>] {
  func d() {
    first
  }
}
