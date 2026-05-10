// {"kind":"complete","languageMode":6,"original":"f8d0cf29","signature":"(anonymous namespace)::ImplicitSelfUsageChecker::isClosureRequiringSelfQualification(swift::AbstractClosureExpr const*, bool)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->","signatureNext":"ImplicitSelfUsageChecker::selfDeclAllowsImplicitSelf"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -swift-version 6 -source-filename %s
actor a {
  c: String {
    {
      func async {
        let b = {
          c
        }
        @#^^#
