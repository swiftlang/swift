// {"kind":"complete","signature":"swift::ConformanceLookupTable::compareConformances(swift::ConformanceLookupTable::ConformanceEntry*, swift::ConformanceLookupTable::ConformanceEntry*, bool&)","useSourceOrderCompletion":true}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s -source-order-completion
class a {
  #^^#
}
extension a? : RandomAccessCollection {
  #^b^#
