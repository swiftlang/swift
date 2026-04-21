// {"kind":"complete","original":"ae634063","signature":"abortWithVerificationError"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype
    b:
      c
    d,
    : {
    }
}
protocol e: a {
  .=
  struct f {
    g {
    #^^#
