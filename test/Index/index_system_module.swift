// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/my_system_overlay/my_system_overlay.swift -Xcc -I -Xcc %S/Inputs/my_system_overlay
// RUN: %target-swift-ide-test -print-indexed-symbols -module-to-print my_system_overlay -source-filename %s -I %t -Xcc -I -Xcc %S/Inputs/my_system_overlay > %t.out
// RUN: %FileCheck %s -input-file=%t.out

// CHECK: class/Swift | SubCls | [[SUBCLS_USR:.*]] | Def | rel: 0
// CHECK: class/Swift | BaseCls | [[BASECLS_USR:.*]] | Ref,RelBase | rel: 1
// CHECK-NEXT: RelBase | SubCls | [[SUBCLS_USR]]
// CHECK: instance-method/Swift | theMeth() | [[SUBCLSMETH_USR:.*]] | Def,RelChild,RelOver | rel: 2
// CHECK-NEXT: RelOver | theMeth() | [[BASECLSMETH_USR:.*]]
// CHECK-NEXT: RelChild | SubCls | [[SUBCLS_USR]]
// CHECK: class/Swift | BaseCls | [[BASECLS_USR]] | Def | rel: 0
// CHECK: instance-method/Swift | theMeth() | [[BASECLSMETH_USR]] | Def,RelChild | rel: 1
// CHECK-NEXT: RelChild | BaseCls | [[BASECLS_USR]]
// CHECK: function/Swift | some_func() | [[SOMEFUNC_USR:.*]] | Def | rel: 0
