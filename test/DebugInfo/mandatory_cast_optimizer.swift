// RUN: %target-swift-frontend -Onone -g -emit-sil %s -parse-as-library -module-name a | %FileCheck %s

// At -Onone, a cast into a variable that is never read must still initialize
// it, because the debugger shows the variable.

protocol P {}
extension Int: P {}

// CHECK-LABEL: sil @$s1a7letCastyySiF
// CHECK:       bb0([[LET_X:%[0-9]+]] : $Int):
// CHECK:         [[LET_S:%[0-9]+]] = alloc_stack [lexical] [var_decl] $any P, let, name "s"
// CHECK-NEXT:    [[LET_PAYLOAD:%[0-9]+]] = init_existential_addr [[LET_S]], $Int
// CHECK-NEXT:    store [[LET_X]] to [[LET_PAYLOAD]]
// CHECK-NEXT:    destroy_addr [[LET_S]]
// CHECK-NEXT:    dealloc_stack [[LET_S]]
public func letCast(_ x: Int) {
  let s = x as! P
}

// CHECK-LABEL: sil @$s1a7varCastyySiF
// CHECK:       bb0([[VAR_X:%[0-9]+]] : $Int):
// CHECK:         [[VAR_S:%[0-9]+]] = alloc_stack [lexical] [var_decl] $any P, var, name "s"
// CHECK-NEXT:    [[VAR_PAYLOAD:%[0-9]+]] = init_existential_addr [[VAR_S]], $Int
// CHECK-NEXT:    store [[VAR_X]] to [[VAR_PAYLOAD]]
// CHECK-NEXT:    destroy_addr [[VAR_S]]
// CHECK-NEXT:    dealloc_stack [[VAR_S]]
public func varCast(_ x: Int) {
  var s = x as! P
}
