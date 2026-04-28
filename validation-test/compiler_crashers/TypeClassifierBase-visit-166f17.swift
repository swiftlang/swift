// {"kind":"emit-silgen","original":"44923edd","signature":"(anonymous namespace)::TypeClassifierBase<(anonymous namespace)::LowerType, swift::Lowering::TypeLowering*>::visit(swift::CanType, swift::Lowering::AbstractionPattern, swift::IsTypeExpansionSensitive_t)","signatureNext":"Lowering::TypeConverter::getTypeLowering"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
struct a: Hashable {
}
class b<c> {
  var d: [a: c] = [:]
}
class e {
  func f<c>(for: c.Type) -> b<c> {
  }
}
@available(SwiftStdlib 5.9, *)
struct g<each c> {
  let h: e
  func map<i>(j: (a, repeat each c) -> i) {
    let k: Set<a>
    k.map { l in
      j(
        l,
        repeat h.f(for: (each c).self).d[l]!
      )
    }
  }
}
