// {"kind":"typecheck","original":"51134ef9","signature":"swift::rewriting::RewriteContext::getRelativeSubstitutionSchemaFromType(swift::CanType, llvm::ArrayRef<swift::rewriting::Term>, llvm::SmallVectorImpl<swift::rewriting::Term>&)","signatureAssert":"Assertion failed: (!concreteType->is<PackExpansionType>()), function getRelativeSubstitutionSchemaFromType","signatureNext":"ConcreteTypeMatcher::mismatch"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b : c
  associatedtype d : a where d.b == b
  protocol c {
    associatedtype e
    struct f<each g> : a {
      typealias b = h<repeat each g>
      struct h<each i> : c {
        typealias e = j<repeat each i>
        struct j < each i> {
          func k < i
          : a where i.b.e ==
          j<i>,
          i.d == f>()
        }
      }
    }
  }
}
