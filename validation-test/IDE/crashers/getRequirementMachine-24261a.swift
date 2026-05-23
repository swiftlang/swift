// {"kind":"complete","original":"71e0b41d","signature":"getRequirementMachine"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a() {
  #^^#
}
protocol b {
  *
  #c(Self)
}
