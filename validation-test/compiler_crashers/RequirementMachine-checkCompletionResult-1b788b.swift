// {"kind":"typecheck","original":"92adc9a7","signature":"swift::rewriting::RequirementMachine::checkCompletionResult(swift::rewriting::CompletionResult) const","signatureNext":"AbstractGenericSignatureRequest::evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a<b, c {
  associatedtype b: d
  associatedtype c
}
protocol d {
  associatedtype e: a
  associatedtype f: g
  protocol g {
    associatedtype h: d
    associatedtype i: d
    struct j<k {
      struct l: a {
        struct m
          typealias b = n
          func o -> some a<n, m>
          struct n: d {
            typealias e =
              l
            struct p: g {
              typealias h = n
              typealias i = n
            }
            typealias f = p
