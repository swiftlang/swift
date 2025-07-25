import Foo

#^COMPLETE^#

// REQUIRES: objc_interop
// RUN: %target-swift-ide-test -code-completion -source-filename %s -F %S/../Inputs/libIDE-mock-sdk -code-completion-token=COMPLETE -code-completion-comments=true | %FileCheck %s -check-prefix=CHECK

// CHECK:            Decl[GlobalVar]/OtherModule[Foo]:   fooIntVar[#Int32#];
// CHECK-SAME-LABEL: name=fooIntVar;
// CHECK-SAME:       briefcomment=Aaa. fooIntVar. Bbb.;
// CHECK-SAME:       fullcomment=<Variable file="{{.*}}" line="63" column="12"><Name>fooIntVar</Name><USR>c:@fooIntVar</USR><Declaration>var fooIntVar: Int32</Declaration><Abstract><Para> Aaa.  fooIntVar.  Bbb.</Para></Abstract></Variable>
