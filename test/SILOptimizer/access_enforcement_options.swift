// RUN: %target-swift-frontend -Onone -Xllvm -sil-print-types -emit-sil -parse-as-library %s | %FileCheck %s --check-prefix=CHECK --check-prefix=NONE
// RUN: %target-swift-frontend -Osize -Xllvm -sil-print-types -emit-sil -parse-as-library %s | %FileCheck %s --check-prefix=CHECK --check-prefix=OPT
// RUN: %target-swift-frontend -O -Xllvm -sil-print-types -emit-sil -parse-as-library %s | %FileCheck %s --check-prefix=CHECK --check-prefix=OPT
// RUN: %target-swift-frontend -Ounchecked -Xllvm -sil-print-types -emit-sil -parse-as-library %s | %FileCheck %s --check-prefix=CHECK --check-prefix=UNCHECKED

@inline(never)
func takesInoutAndEscaping(_: inout Int, _ f: @escaping () -> ()) {
  f()
}

@inline(never)
func escapeClosure(_ f: @escaping () -> ()) -> () -> () {
  return f
}

public func accessIntTwice() {
  var x = 0
  takesInoutAndEscaping(&x, escapeClosure({ x = 3 }))
}

// accessIntTwice()
// CHECK-LABEL: sil @$s26access_enforcement_options0A8IntTwiceyyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box ${ var Int }, var, name "x"
// CHECK: [[PROJ:%.*]] = project_box [[BOX]] : ${ var Int }, 0
// NONE: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[PROJ]] : $*Int
// OPT: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[PROJ]] : $*Int
// UNCHECKED-NOT: = begin_access
// CHECK-LABEL: } // end sil function '$s26access_enforcement_options0A8IntTwiceyyF'

// closure #1 in accessIntTwice()
// CHECK-LABEL: sil {{.*}}@$s26access_enforcement_options0A8IntTwiceyyFyycfU_ : $@convention(thin) (@guaranteed { var Int }) -> () {
// CHECK: bb0(%0 : @closureCapture ${ var Int }):
// CHECK: [[PROJ:%.*]] = project_box %0 : ${ var Int }, 0
// NONE: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[PROJ]] : $*Int
// OPT: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[PROJ]] : $*Int
// UNCHECKED-NOT: = begin_access
// CHECK-LABEL: } // end sil function '$s26access_enforcement_options0A8IntTwiceyyFyycfU_'
