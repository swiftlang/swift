// {"kind":"complete","signature":"swift::ConformanceLookupTable::compareConformances(swift::ConformanceLookupTable::ConformanceEntry*, swift::ConformanceLookupTable::ConformanceEntry*, bool&)","signatureAssert":"Assertion failed: (lhs->getDeclContext()->getParentModule() == rhs->getDeclContext()->getParentModule() && \"conformances should be in the same module\"), function compareConformances","useSourceOrderCompletion":true}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-order-completion -source-filename %s
class a {
  #^^#
}
extension a? : RandomAccessCollection {
  #^b^#
