// {"kind":"typecheck","signature":"swift::GenericSignatureImpl::getReducedTypeParameter(swift::CanType) const","signatureNext":"GenericEnvironment::getOrCreateArchetypeFromInterfaceType"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
enum b : c protocol
a{typealias c : d.e struct d : a func c typealias e} typealias c : a
