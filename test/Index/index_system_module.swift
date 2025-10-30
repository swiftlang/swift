// RUN: %empty-directory(%t)
//
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/my_system_overlay/my_system_overlay.swift -Xcc -I -Xcc %S/Inputs/my_system_overlay
// RUN: %target-swift-ide-test -print-indexed-symbols -module-to-print my_system_overlay -source-filename %s -I %t -Xcc -I -Xcc %S/Inputs/my_system_overlay > %t.out
// RUN: %FileCheck %s -input-file=%t.out
// RUN: %FileCheck -check-prefix NEGATIVE %s -input-file=%t.out

// CHECK: function(public)/Swift | some_func() | [[SOMEFUNC_USR:.*]] | Def | rel: 0
// CHECK: class(public)/Swift | BaseCls | [[BASECLS_USR:.*]] | Def | rel: 0
// CHECK: instance-method(public)/Swift | theMeth() | [[BASECLSMETH_USR:.*]] | Def,Dyn,RelChild | rel: 1
// CHECK-NEXT: RelChild | class/Swift | BaseCls | [[BASECLS_USR]]
// CHECK: class(public)/Swift | SubCls | [[SUBCLS_USR:.*]] | Def | rel: 0
// CHECK: class/Swift | BaseCls | [[BASECLS_USR]] | Ref,RelBase | rel: 1
// CHECK-NEXT: RelBase | class/Swift | SubCls | [[SUBCLS_USR]]
// CHECK: instance-method(public)/Swift | theMeth() | [[SUBCLSMETH_USR:.*]] | Def,Dyn,RelChild,RelOver | rel: 2
// CHECK-NEXT: RelOver | instance-method/Swift | theMeth() | [[BASECLSMETH_USR]]
// CHECK-NEXT: RelChild | class/Swift | SubCls | [[SUBCLS_USR]]

// NEGATIVE-NOT: SECRET
