// {"kind":"typecheck","signature":"swift::Mangle::ASTMangler::appendConcreteProtocolConformance(swift::ProtocolConformance const*, swift::GenericSignature)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension ManagedBuffer: Hashable {}
func a() -> Set<some ManagedBuffer<Int, Int>> {}
