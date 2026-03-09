// {"kind":"typecheck","original":"2889e412","signature":"swift::constraints::Constraint::Constraint(swift::constraints::ConstraintKind, swift::constraints::ConversionRestrictionKind, swift::Type, swift::Type, swift::constraints::ConstraintLocator*, llvm::SmallPtrSetImpl<swift::TypeVariableType*>&)","signatureAssert":"Assertion failed: (isAdmissibleType(first)), function Constraint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  associatedtype c
  func d() -> (e: b, f: c)
  struct j<g> {
    > {
    struct h : a { typealias b = String typealias c = i func d -> (e: String f: i
    }
    }
    struct i {
    }
  }
}
