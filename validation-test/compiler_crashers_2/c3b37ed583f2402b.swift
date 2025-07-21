// {"kind":"typecheck","signature":"(anonymous namespace)::IsBindableVisitor::handleGenericNominalType(swift::NominalTypeDecl*, swift::CanType, swift::CanType)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a < b protocol
    c{associatedtype f : a<d> associatedtype d : a<f>} func e < b : c {
  b.f = b.d
