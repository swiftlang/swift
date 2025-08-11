// {"kind":"emit-silgen","signature":"swift::CanTypeVisitor<swift::Lowering::TypeConverter::computeLoweredRValueType(swift::TypeExpansionContext, swift::Lowering::AbstractionPattern, swift::CanType)::LoweredRValueTypeVisitor, swift::CanType>::visit(swift::CanType)"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a {
  func b() -> _
}
do {
  func c(d : a) {
    let e = d.b
  }
}
