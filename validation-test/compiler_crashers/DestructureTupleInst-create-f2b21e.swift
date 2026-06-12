// {"kind":"emit-silgen","original":"bc3658dc","signature":"swift::DestructureTupleInst::create(swift::SILFunction const&, swift::SILDebugLocation, swift::SILValue, swift::ValueOwnershipKind)","signatureAssert":"Assertion failed: (Operand->getType().is<TupleType>() && \"Expected a tuple typed operand?!\"), function create","signatureNext":"PatternMatchEmission::emitDestructiveCaseBlocks::ConsumingPatternBindingVisitor::visitEnumProjection"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
enum a<b: ~Copyable> {
  case c
  indirect case d(b, a)
  case j(b)
}
extension a: Copyable where b: Copyable {
}
enum e<b: ~Copyable, i>: ~Copyable {
  case k(i, a<b>)
  func f() -> b? {
    switch self {
    case .k(_, let g):
      switch g {
      case .c:
        return nil
      case .d(let h, _):
        h
      case .j(let h):
        return h
      }
    }
  }
}
