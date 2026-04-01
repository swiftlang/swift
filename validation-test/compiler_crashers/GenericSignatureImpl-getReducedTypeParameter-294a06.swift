// {"kind":"typecheck","signature":"swift::GenericSignatureImpl::getReducedTypeParameter(swift::CanType) const","signatureNext":"GenericEnvironment::getOrCreateArchetypeFromInterfaceType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum b : c protocol
a{typealias c : d.e struct d : a func c typealias e} typealias c : a
