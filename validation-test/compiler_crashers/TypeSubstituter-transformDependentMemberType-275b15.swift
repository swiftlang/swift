// {"kind":"typecheck","signature":"(anonymous namespace)::TypeSubstituter::transformDependentMemberType(swift::DependentMemberType*, swift::TypePosition)","stackOverflow":true}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a: Collection where Iterator == Self
  struct b<c: a>: IteratorProtocol {next -> c.Element? struct c: a {
      struct Element
        subscript(Int) Element
        func makeIterator -> b<Self>
