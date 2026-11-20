// {"kind":"complete","original":"adb47d2b","signature":"swift::TypeDecl::getName() const","signatureAssert":"Assertion failed: (hint == nullptr), function addMemberSilently"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@propertyWrapper struct a
  extension {
    var : {
      struct b {
        @a c: = #^^#
