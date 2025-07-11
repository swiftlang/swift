import Foo

// REQUIRES: objc_interop
// RUN: %sourcekitd-test -req=complete -req-opts=includefulldocumentation=1 -pos=2:1 %s -- -F %S/../Inputs/libIDE-mock-sdk %s | %FileCheck %s

// CHECK-LABEL:      key.name: "fooIntVar",
// CHECK-NEXT:       key.doc.full_as_xml: "<Variable file=\"{{.*}}\" line=\"{{.*}}\" column=\"{{.*}}\"><Name>fooIntVar</Name><USR>c:@fooIntVar</USR><Declaration>var fooIntVar: Int32</Declaration><Abstract><Para> Aaa. fooIntVar. Bbb.</Para></Abstract></Variable>",
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

