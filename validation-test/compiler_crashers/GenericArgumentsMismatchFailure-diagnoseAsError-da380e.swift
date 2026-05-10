// {"kind":"typecheck","original":"0e1c3fed","signature":"swift::constraints::GenericArgumentsMismatchFailure::diagnoseAsError()","signatureAssert":"Assertion failed: (purpose != CTP_Unused), function diagnoseAsError","signatureNext":"GenericArgumentsMismatch::diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@resultBuilder enum a {
  struct b<c
    static func buildExpression( Int) -> b<String>
    static func buildPartialBlock<c>(first: b<c>) -> b<c>
    static func buildPartialBlock(accumulated b next b
    >
    static func buildArray<c>( [b<c>]) -> b<[c]>
    static func buildFinalResult<c>(b      < c >) -> c
    static func buildFinalResult<c>(b      < c >)
    func assemble<c>(@a  () -> c) -> c
    func d -> [[Int]] {
      assemble {
        for   0..<4 {
          for col  0..<3 {
            col
