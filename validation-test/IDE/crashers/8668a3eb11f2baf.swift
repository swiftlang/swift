// {"kind":"complete","signature":"swift::ide::USRBasedType::fromType(swift::Type, swift::ide::USRBasedTypeArena&)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a: c {  b(a  = #^^# }
protocol c: a
