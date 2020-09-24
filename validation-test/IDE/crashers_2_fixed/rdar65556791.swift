// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// RUN: %target-swift-ide-test -code-completion -code-completion-token=B -source-filename=%s
// RUN: %target-swift-ide-test -code-completion -code-completion-token=C -source-filename=%s

func myFunc(_: Int, _: Undefined) {}

undefined {
  myFunc($0, undefined)
} #^A^#

undefined(x: 1) {
  myFunc($0, undefined)
} #^B^#

undefined(1, 2) {
  myFunc { 1 }
} #^C^#
