// {"kind":"typecheck","signature":"swift::IsStaticRequest::evaluate(swift::Evaluator&, swift::FuncDecl*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
@objcMembers class a open extension a {
  ...
