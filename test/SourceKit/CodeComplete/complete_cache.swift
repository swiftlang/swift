import Foo

// RUN: %sourcekitd-test -req=complete.cache.ondisk -cache-path=%t.ccp == \
// RUN:     -req=complete.open -pos=2:1 -req-opts=hidelowpriority=0 %s -- %s -F %S/../Inputs/libIDE-mock-sdk > %t.completions1

// Make sure we built the cache.
// RUN: ls %t.ccp | grep "Foo.*completions"

// RUN: %sourcekitd-test -req=complete.cache.ondisk -cache-path=%t.ccp == \
// RUN:     -req=complete -pos=2:1 %s -- %s -F %S/../Inputs/libIDE-mock-sdk > %t.completions2

// Sanity check the results
// RUN: FileCheck %s < %t.completions1
// RUN: FileCheck %s < %t.completions2
// CHECK: key.name: "FooStruct
