// {"kind":"emit-ir","signature":"swift::irgen::emitPhysicalStructMemberFixedOffset(swift::irgen::IRGenModule&, swift::SILType, swift::VarDecl*)","signatureNext":"emitKeyPathComponent"}
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
func g() -> KeyPath<c<f>, c<f>> {
  \.e
}
