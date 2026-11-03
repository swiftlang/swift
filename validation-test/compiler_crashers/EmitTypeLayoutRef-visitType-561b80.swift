// {"kind":"emit-ir","signature":"(anonymous namespace)::EmitTypeLayoutRef::visitType(swift::CanType, swift::irgen::DynamicMetadataRequest)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -emit-ir %s
protocol a {
  associatedtype b
}
struct c<d: a> {
  var e: d.b
}
struct f: a {
  typealias b = c<f>
}
class g {
  var e: c<f>
  init(e: c<f>) {
    self.e = e
  }
}
