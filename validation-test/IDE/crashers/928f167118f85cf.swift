// {"kind":"complete","original":"7bbb1dc5","signature":"swift::TypeRepr::print(swift::ASTPrinter&, swift::PrintOptions const&, swift::optionset::OptionSet<swift::NonRecursivePrintOption, unsigned int>) const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
a ? {
init { b { extension
}#^^#
