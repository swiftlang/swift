// RUN: %target-swift-frontend -emit-sil -verify %s

// Verify that CapturePromotion does not crash when remapping the substitution
// map of a partial_apply whose conformances include a pack conformance.
//
// Cloning the closure below remaps a partial_apply of '!=' that carries a pack
// conformance for 'Pack{repeat each V} : Equatable'. Substituting that pack
// conformance enters an active pack expansion; the cloner's no-substitution-map
// conformance fallback must not project a pack element lane onto the conforming
// type, which previously produced an invalid PackElementType subject and
// aborted in ProtocolConformanceRef::forAbstract.
//
// rdar://175912818

public func crash<each V: Equatable & Sendable>(
  action: sending @escaping (repeat (each V, Bool)) -> ()
) -> ((repeat each V)) -> ()
{
  { o in
    action(repeat (each o, false || (each o != each o)))
  }
}
