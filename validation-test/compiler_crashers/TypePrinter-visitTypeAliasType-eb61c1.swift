// {"kind":"typecheck","original":"f584f943","signature":"(anonymous namespace)::TypePrinter::visitTypeAliasType(swift::TypeAliasType*, swift::optionset::OptionSet<swift::NonRecursivePrintOption, unsigned int>)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  typealias b<c> = a where d == b<c>.e
}
