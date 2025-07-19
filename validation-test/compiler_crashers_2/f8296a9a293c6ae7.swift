// {"signature":"void swift::Diagnostic::gatherArgs<swift::Type>(swift::Type)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
class a open extension a {
  @objc b : Int &
