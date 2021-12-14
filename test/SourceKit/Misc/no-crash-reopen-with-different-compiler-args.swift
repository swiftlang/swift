// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// This used to crash with a nullptr dereference because we didn't store a 
// snapshot in the FileContents of primary.swift when it is opened for the first 
// time but we are trying to access the snapshot when trying determining if we 
// can reuse the AST for the cursor info request

// RUN: %sourcekitd-test \
// RUN: -req=open %t/primary.swift -- %t/primary.swift %t/secondary.swift \
// RUN: == -req=close %t/primary.swift  \
// RUN: == -req=open %t/primary.swift -- %t/primary.swift \
// RUN: == -req=cursor -pos 2:8 %t/primary.swift -- %t/primary.swift %t/secondary.swift \
// RUN: | %FileCheck %s

// BEGIN primary.swift

struct Foo {}
// CHECK: source.lang.swift.decl.struct
// CHECK-NEXT: Foo
// CHECK-NEXT: s:4main3FooV

// BEGIN secondary.swift