// {"kind":"complete","original":"d51437eb","signature":"swift::Decl::getResolvedCustomAttrType(swift::CustomAttr*) const","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension FixedWidthInteger {
  @propertyWrapper struct a {
    typealias b<c> = a
    typealias d = b<String>
    @d var e
    #^^#
  }
}
