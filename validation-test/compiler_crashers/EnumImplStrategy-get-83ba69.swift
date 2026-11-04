// {"kind":"emit-ir","signature":"swift::irgen::EnumImplStrategy::get(swift::irgen::TypeConverter&, swift::SILType, swift::EnumDecl*)","signatureNext":"TypeConverter::convertEnumType"}
// RUN: not --crash %target-swift-frontend -emit-ir %s
protocol a {
  associatedtype b
}
enum c<d: a> {
  case e(d.b)
}
struct f: a {
  typealias b = c<f>
}
func g(h: c<f>) {
}
