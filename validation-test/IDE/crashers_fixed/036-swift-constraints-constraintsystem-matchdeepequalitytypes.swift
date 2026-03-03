// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s -code-completion-verify-usr-to-decl=false
extension{enum B{enum B{enum S{func c
let t=c{#^A^#
