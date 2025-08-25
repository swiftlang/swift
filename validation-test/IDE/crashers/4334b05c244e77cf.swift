// {"kind":"complete","original":"1a6ddccd","signature":"getRequirementMachine"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype c: a
  var b: c
}
protocol d: a where c == Never
  extension Never: d
    extension d {
      public( #^^#
      b: Never
