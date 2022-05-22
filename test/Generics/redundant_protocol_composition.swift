// RUN: %target-typecheck-verify-swift -warn-redundant-requirements

// The GenericSignatureBuilder did not diagnose the first two.

struct G1<T : BinaryInteger & FixedWidthInteger & UnsignedInteger & Codable> {}
// expected-warning@-1 {{redundant conformance constraint 'T' : 'BinaryInteger'}}

struct G2<T> where T : BinaryInteger & FixedWidthInteger & UnsignedInteger & Codable {}
// expected-warning@-1 {{redundant conformance constraint 'T' : 'BinaryInteger'}}

struct G3<T> where T : BinaryInteger, T : FixedWidthInteger, T : UnsignedInteger & Codable {}
// expected-warning@-1 {{redundant conformance constraint 'T' : 'BinaryInteger'}}

// FIXME(rqm-diagnostics): These should also diagnose, but do not due to overly-eager
// canonicalization in ProtocolCompositionType::get().

struct G1a<T : BinaryInteger & FixedWidthInteger & UnsignedInteger> {}
struct G2a<T> where T : BinaryInteger & FixedWidthInteger & UnsignedInteger {}

