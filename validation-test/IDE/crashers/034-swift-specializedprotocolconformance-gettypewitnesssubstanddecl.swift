// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
struct A<c{class a<A{enum S:CollectionType
#^A^#