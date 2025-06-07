// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -module-name main -import-objc-header %S/Inputs/open_enum.h -experimental-skip-non-inlinable-function-bodies | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -module-name main -import-objc-header %S/Inputs/open_enum.h -experimental-skip-non-inlinable-function-bodies-without-types | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -module-name main -import-objc-header %S/Inputs/open_enum.h -debug-forbid-typecheck-prefix SKIP_ALL_NO_TYPECHECK -experimental-skip-all-function-bodies | %FileCheck %s --check-prefix=CHECK-SKIP-ALL

// CHECK-SKIP-ALL-NOT: s4main13inlinableFuncSo7YesOrNoVyF

// CHECK: sil [serialized]{{.*}} @$s4main13inlinableFuncSo7YesOrNoVyF : $@convention(thin) () -> YesOrNo {
// CHECK:   return {{%.*}} : $YesOrNo
// CHECK: } // end sil function '$s4main13inlinableFuncSo7YesOrNoVyF'
@inlinable public func inlinableFunc() -> YesOrNo {
  let SKIP_ALL_NO_TYPECHECK = 1
  _ = SKIP_ALL_NO_TYPECHECK
  return YesOrNo(rawValue: 1)!
}

// CHECK-SKIP-ALL-NOT: sSo7YesOrNoV8rawValueABSgs5Int32V_tcfC

// CHECK: sil shared [serialized]{{.*}} @$sSo7YesOrNoV8rawValueABSgs5Int32V_tcfC : $@convention(method) (Int32, @thin YesOrNo.Type) -> Optional<YesOrNo> {
// CHECK:   return {{%.*}} : $Optional<YesOrNo>
// CHECK: } // end sil function '$sSo7YesOrNoV8rawValueABSgs5Int32V_tcfC'
