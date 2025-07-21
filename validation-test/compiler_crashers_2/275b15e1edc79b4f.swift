// {"kind":"typecheck","signature":"swift::ProtocolConformanceRef::getTypeWitness(swift::AssociatedTypeDecl*, swift::SubstOptions) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a: Collection where Iterator == Self
  struct b<c: a>: IteratorProtocol {next -> c.Element? struct c: a {
      struct Element
        subscript(Int) Element
        func makeIterator -> b<Self>
