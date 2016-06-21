// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
var b{protocol A{enum B<e{func a{struct c<T where T.h:A{#^A^#
