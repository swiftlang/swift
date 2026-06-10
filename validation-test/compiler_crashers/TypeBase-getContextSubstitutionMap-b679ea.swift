// {"kind":"emit-sil","original":"c4899854","signature":"swift::TypeBase::getContextSubstitutionMap()","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"Lowering::TypeConverter::getBoxTypeForEnumElement"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
struct a<b, c> {
  enum d {
    case e
    case f(c)
    indirect case h(b, c, d)
    consuming func i() -> c? {
      switch self {
      case .e:
        nil
      case .f(let g):
        g
      case .h(_, let g, _):
        g
      }
    }
  }
}
