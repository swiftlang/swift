// RUN: %empty-directory(%t)
//
// RUN: %target-swift-frontend -emit-module -o %t %s
// RUN: %target-swift-ide-test -print-indexed-symbols -module-name index_module -source-filename %s > %t.out
// RUN: %target-swift-ide-test -print-indexed-symbols -module-to-print index_module -source-filename %s -I %t >> %t.out
// RUN: %FileCheck %s -input-file=%t.out

public var someGlobal: Int = 0
// CHECK: [[@LINE-1]]:12 | variable/Swift | someGlobal | [[SOMEGLOBAL_USR:.*]] | Def | rel: 0
// CHECK: [[@LINE-2]]:12 | function/acc-get/Swift | getter:someGlobal | [[SOMEGLOBAL_GET_USR:.*]] | Def,Impl,RelChild,RelAcc | rel: 1
// CHECK-NEXT:   RelChild,RelAcc | variable/Swift | someGlobal | [[SOMEGLOBAL_USR]]
// CHECK: [[@LINE-4]]:12 | function/acc-set/Swift | setter:someGlobal | [[SOMEGLOBAL_SET_USR:.*]] | Def,Impl,RelChild,RelAcc | rel: 1
// CHECK-NEXT:   RelChild,RelAcc | variable/Swift | someGlobal | [[SOMEGLOBAL_USR]]

public func someFunc() {}
// CHECK: [[@LINE-1]]:13 | function/Swift | someFunc() | [[SOMEFUNC_USR:.*]] | Def | rel: 0

public class CCC {}
// CHECK: [[@LINE-1]]:14 | class/Swift | CCC | [[CCC_USR:.*]] | Def | rel: 0
// CHECK: [[@LINE-2]]:14 | constructor/Swift | init() | [[CCC_init_USR:.*]] | Def,Impl,RelChild | rel: 1
// CHECK-NEXT:   RelChild | class/Swift | CCC | [[CCC_USR]]

public typealias GenericTypealias<T> = Optional<T>
// CHECK: [[@LINE-1]]:18 | type-alias/Swift | GenericTypealias | [[TYPEALIAS_USR:.*]] | Def | rel: 0
// CHECK: [[@LINE-2]]:35 | type-alias/generic-type-param/Swift | T | s:12index_module16GenericTypealiasa1Txmfp | Def,RelChild | rel: 1
// CHECK-NEXT:   RelChild | type-alias/Swift | GenericTypealias | [[TYPEALIAS_USR]]

// --- Check the module ---

// CHECK: 0:0 | variable/Swift | someGlobal | [[SOMEGLOBAL_USR]] | Def | rel: 0
// CHECK: 0:0 | function/acc-get/Swift | getter:someGlobal | [[SOMEGLOBAL_GET_USR:.*]] | Def,Impl,RelChild,RelAcc | rel: 1
// CHECK-NEXT:   RelChild,RelAcc | variable/Swift | someGlobal | [[SOMEGLOBAL_USR]]
// CHECK: 0:0 | function/acc-set/Swift | setter:someGlobal | [[SOMEGLOBAL_SET_USR:.*]] | Def,Impl,RelChild,RelAcc | rel: 1
// CHECK-NEXT:   RelChild,RelAcc | variable/Swift | someGlobal | [[SOMEGLOBAL_USR]]

// CHECK: 0:0 | function/Swift | someFunc() | [[SOMEFUNC_USR]] | Def | rel: 0

// CHECK: 0:0 | class/Swift | CCC | [[CCC_USR]] | Def | rel: 0
// CHECK-NOT: constructor/Swift

// CHECK: 0:0 | type-alias/Swift | GenericTypealias | [[TYPEALIAS_USR]] | Def | rel: 0
