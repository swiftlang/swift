// {"kind":"emit-ir","signature":"swift::irgen::EnumImplStrategy::get(swift::irgen::TypeConverter&, swift::SILType, swift::EnumDecl*)","signatureNext":"TypeConverter::convertEnumType"}
// RUN: not --crash %target-swift-frontend -emit-ir %s
protocol a {
  associatedtype b
}
enum c<d: a> {
  case e(d.b)
  case f(Int)
  case k
}
struct g: a {
  typealias b = c<g>
}
func h(j: c<g>) -> Int {
  switch j {
  case .e, .k:
    return 0
  case .f(let i):
    return i
  }
}
