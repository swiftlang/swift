import Foo

// REQUIRES: objc_interop
// RUN: rm -rf %t.ccp
// RUN: %sourcekitd-test -req=complete.cache.ondisk -cache-path=%t.ccp == \
// RUN:     -req=complete.open -pos=2:1 -req-opts=hidelowpriority=0 %s -- %s -F %S/../Inputs/libIDE-mock-sdk > %t.completions1

// Make sure we built the cache.
// RUN: ls %t.ccp | grep "Foo.*completions"

// RUN: %sourcekitd-test -req=complete.cache.ondisk -cache-path=%t.ccp == \
// RUN:     -req=complete -pos=2:1 %s -- %s -F %S/../Inputs/libIDE-mock-sdk > %t.completions2

// Soundness check the results
// RUN: %FileCheck %s < %t.completions1
// RUN: %FileCheck %s < %t.completions2
// CHECK: key.name: "FooStruct

// RUN: %complete-test -raw -tok=VOID_1 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=VOID_1
// RUN: %complete-test -raw -tok=VOID_2 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=VOID_1
// RUN: %complete-test -raw -tok=VOID_3 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=VOID_3
func test1() {
  _ = #^VOID_1,fooFunc^#
}
func test2() {
  for i in 1...#^VOID_2,fooFunc^# {}
}
// VOID_1: key.name: "fooFuncNoreturn1()",
// VOID_1-NEXT: key.description:
// VOID_1-NEXT: key.typename: "Never",
// VOID_1-NEXT: key.context: source.codecompletion.context.othermodule,
// VOID_1-NEXT: key.moduleimportdepth: 1,
// VOID_1-NEXT: key.num_bytes_to_erase: 0,
// VOID_1-NEXT: key.substructure:
// VOID_1: key.sourcetext:

// VOID_1: key.name: "fooHelperSubFunc1(:)",
// VOID_1-NEXT: key.description:
// VOID_1-NEXT: key.typename: "Int32",
// VOID_1-NEXT: key.context: source.codecompletion.context.othermodule,
// VOID_1-NEXT: key.moduleimportdepth: 2,
// VOID_1-NEXT: key.num_bytes_to_erase: 0,
// VOID_1-NEXT: key.substructure:
// VOID_1: key.sourcetext:

func test3() {
  #^VOID_3,fooFunc^# {}
}
// VOID_3-NOT: key.not_recommended
