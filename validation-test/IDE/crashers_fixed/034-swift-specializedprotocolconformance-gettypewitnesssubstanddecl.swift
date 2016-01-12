// RUN: not %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s

// Test is just disabled to unblock the bots. TODO: enable the test and fix it.

// REQUIRES: FIXME

struct A<c{class a<A{enum S:CollectionType
#^A^#
