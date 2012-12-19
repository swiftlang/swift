// RUN: %swift -parse-as-library -dump-sil %s | FileCheck %s

protocol Unloadable {}

// CHECK: func_decl address_only_argument
func address_only_argument(x:Unloadable) {
  // CHECK: bb0([[XARG:%[0-9]+]] : [byref] Unloadable):
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Unloadable
  // CHECK: copy_addr [[XARG]] to [[XBOX]]#1 [initialization]
  // CHECK: release [[XBOX]]
}

// CHECK: func_decl address_only_return
func address_only_return(x:Unloadable, y:Int) -> Unloadable {
  // CHECK: bb0([[XARG:%[0-9]+]] : [byref] Unloadable, [[YARG:%[0-9]+]] : Int64, [[RET:%[0-9]+]] : [byref] Unloadable):
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Unloadable
  // CHECK: copy_addr [[XARG]] to [[XBOX]]#1 [initialization]
  return x
  // CHECK: copy_addr [[XBOX]]#1 to [[RET]]
  // CHECK: [[VOID:%[0-9]+]] = tuple ()
  // CHECK: return ([[VOID]])
}

