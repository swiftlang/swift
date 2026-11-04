// {"kind":"emit-ir","signature":"swift::irgen::EnumImplStrategy::get(swift::irgen::TypeConverter&, swift::SILType, swift::EnumDecl*)","signatureNext":"TypeConverter::convertEnumType"}
// RUN: not --crash %target-swift-frontend -emit-ir %s
protocol a {
  associatedtype b
}
enum c<d: a> {
  case f(d.b)
}
struct g: a {
  typealias b = c<g>
}
class h {
  var e: c<g>
  init(e: c<g>) {
    self.e = e
  }
}
