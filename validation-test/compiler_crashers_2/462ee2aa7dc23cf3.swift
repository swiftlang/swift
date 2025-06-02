// {"signature":"recordTypeWitness(swift::NormalProtocolConformance*, swift::AssociatedTypeDecl*, swift::Type, swift::TypeDecl*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a{associatedtype b} protocol c
    : a{d(b)} protocol e{associatedtype f} extension e {
  protocol g : e, c { associatedtype f : c associatedtype b }
  struct h<i> : g {
    typealias f = i
