// {"kind":"emit-silgen","original":"bc3658dc","signature":"(anonymous namespace)::PatternMatchEmission::bindIrrefutableBorrows((anonymous namespace)::ClauseRow const&, llvm::ArrayRef<swift::Lowering::ConsumableManagedValue>, bool, bool)","signatureAssert":"Assertion failed: (value.getFinalConsumption() == CastConsumptionKind::BorrowAlways), function bindBorrow","signatureNext":"PatternMatchEmission::emitDispatch"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
enum a<b: ~Copyable> {
  case c
  indirect case d(b)
}
extension a: Copyable where b: Copyable {
}
enum e<b: ~Copyable, i>: ~Copyable {
  case j(i, a<b>)
  func f() -> b? {
    switch self {
    case .j(_, let g):
      switch g {
      case .c:
        nil
      case .d(let h):
        h
      }
    }
  }
}
