// {"signature":"swift::IsObjCRequest::evaluate(swift::Evaluator&, swift::ValueDecl*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
class a {
  @objc var b : Int {
    get throws {
    }
    set
  }
}
