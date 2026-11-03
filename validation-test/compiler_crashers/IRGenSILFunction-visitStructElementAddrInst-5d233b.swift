// {"kind":"emit-ir","signature":"(anonymous namespace)::IRGenSILFunction::visitStructElementAddrInst(swift::StructElementAddrInst*)","signatureNext":"IRGenSILFunction::emitSILFunction"}
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
func h(g: c<f>) -> c<f> {
  g.e
}
