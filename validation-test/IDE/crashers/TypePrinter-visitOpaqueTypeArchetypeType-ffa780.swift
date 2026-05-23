// {"kind":"complete","original":"09ff02fc","signature":"(anonymous namespace)::TypePrinter::visitOpaqueTypeArchetypeType(swift::OpaqueTypeArchetypeType*, swift::optionset::OptionSet<swift::NonRecursivePrintOption, unsigned int>)","stackOverflow":true}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b
  func c -> b
}
protocol d: a
  extension d {
    c -> some
      Collection<b>
    struct e: d {
      #^^#
