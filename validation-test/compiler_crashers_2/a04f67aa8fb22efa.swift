// {"signature":"checkRequirementsImpl(llvm::ArrayRef<swift::Requirement>, bool)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < Element {
  b {
    class c : Collection { typealias Index = d subscript(e :
