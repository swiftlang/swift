// {"signature":"void swift::Diagnostic::gatherArgs<swift::Identifier, bool, swift::TypeRepr*>(swift::Identifier, bool, swift::TypeRepr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
@objcMembers class a open extension a {
  ...
