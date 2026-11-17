// {"kind":"emit-sil","signature":"swift::Lowering::TypeConverter::getTypeSubElementCount(swift::SILType, swift::TypeExpansionContext)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -emit-sil %s
protocol a {
  associatedtype b
}
struct c<d: a> {
  var e: d.b
}
struct f: a {
  typealias b = c<f>
}
func g(h: borrowing c<f>) {
}
