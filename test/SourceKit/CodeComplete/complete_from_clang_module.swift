import Foo

// REQUIRES: objc_interop
// RUN: %sourcekitd-test -req=complete -pos=2:1 %s -- -F %S/../Inputs/libIDE-mock-sdk %s | %FileCheck %s

// CHECK-LABEL:      key.name: "fooIntVar",
// CHECK-NEXT:       key.description: "fooIntVar",
// CHECK-NEXT:       key.typename: "Int32",
// CHECK-NEXT:       key.doc.brief: "Aaa.  fooIntVar.  Bbb.",
// CHECK-NEXT:       key.context: source.codecompletion.context.othermodule,
// CHECK-NEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// CHECK-NEXT:       key.num_bytes_to_erase: 0,
// CHECK-NEXT:       key.associated_usrs: "c:@fooIntVar",
// CHECK-NEXT:       key.modulename: "Foo",
// CHECK-NEXT:       key.sourcetext: "fooIntVar"
// CHECK-NEXT:     },

