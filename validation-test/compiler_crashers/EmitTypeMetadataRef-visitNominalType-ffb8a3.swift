// {"kind":"emit-ir","original":"7b6e1294","signature":"(anonymous namespace)::EmitTypeMetadataRef::visitNominalType(swift::CanTypeWrapper<swift::NominalType>, swift::irgen::DynamicMetadataRequest)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -emit-ir %s
protocol c {
  associatedtype d: Hashable
}
protocol g {
  associatedtype k
  associatedtype h: c
  associatedtype l where h == k, l == Self
}
protocol n<o, p> {
  associatedtype o
  associatedtype p: g where p.h == o
  associatedtype aa: Collection where aa.Element == p
}
struct r<d: Hashable>: c {
  func i(j: r) -> s<d> {
    s()
  }
}
struct s<d: Hashable>: g {
  typealias k = r<d>
}
struct t<ac: n, x> {
  var ad: ac
  var ae: x
  func q(ab: ac.o, j: ac.o) -> [ac.p] {
    []
  }
}
func ap<aq>(
  ar: any n<aq, s<aq>>,
  secondary: any n,
  src: r<aq>,
  dst: r<aq>
) -> Double {
  func z<T1: n, y>(
    _ a: T1,
    _ b: y
  ) -> Double
  where T1.p == s<aq> {
    let combined = t(ad: a, ae: b)
    let ao = combined.q(ab: src, j: dst)
    return ao.reduce(0.0) { _, _ in
      0
    }
  }
  return z(ar, secondary)
}
