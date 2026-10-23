// {"kind":"typecheck","signature":"(anonymous namespace)::IsBindableVisitor::visitArchetypeType(swift::ArchetypeType*, swift::CanType)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a < b protocol
    c{associatedtype f : a<d> associatedtype d : a<f>} func e < b : c {
  b.f = b.d
