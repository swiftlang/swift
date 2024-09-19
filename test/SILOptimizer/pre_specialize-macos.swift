// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -O -swift-version 5 -enable-library-evolution -emit-module -o /dev/null -emit-module-interface-path %t/pre_specialized_module.swiftinterface %S/Inputs/pre_specialized_module.swift -module-name pre_specialized_module
// RUN: %target-swift-frontend -I %t -O -swift-version 5 -enable-library-evolution -emit-module -o /dev/null -emit-module-interface-path %t/pre_specialized_module2.swiftinterface %S/Inputs/pre_specialized_module2.swift -module-name pre_specialized_module2
// RUN: %target-swift-frontend -I %t -O -emit-sil -target %target-cpu-apple-macos51 %s | %FileCheck %s --check-prefix=OPT --check-prefix=CHECK
// RUN: %target-swift-frontend -I %t -O -emit-sil -target %target-cpu-apple-macosx49 %s | %FileCheck %s --check-prefix=NOOPT --check-prefix=CHECK

// REQUIRES: OS=macosx && CPU=x86_64

import pre_specialized_module
import pre_specialized_module2

// CHECK: sil @$s4main28usePrespecializedEntryPointsyyF : $@convention(thin) () -> () {
// OPT:  [[F1:%.*]] = function_ref @$s22pre_specialized_module21publicPrespecialized2yyxlFAA8SomeDataV_Ts5 : $@convention(thin) (SomeData) -> ()
// OPT:  apply [[F1]](
// OPT:  [[F2:%.*]] = function_ref @$s22pre_specialized_module21publicPrespecialized2yyxlF0a1_B8_module213SomeOtherDataV_Ts5 : $@convention(thin) (SomeOtherData) -> ()
// OPT:  apply [[F2]](
// In the no prespecialization case we get regular generic specialization
// because the function is inlinable.
// NOOPT:  [[F1:%.*]] = function_ref @$s22pre_specialized_module21publicPrespecialized2yyxlFAA8SomeDataV_Tg5Tf4d_n : $@convention(thin) () -> ()
// NOOPT:  apply [[F1]]()
// NOOPT:  [[F2:%.*]] = function_ref @$s22pre_specialized_module21publicPrespecialized2yyxlF0a1_B8_module213SomeOtherDataV_Tg5Tf4d_n : $@convention(thin) () -> ()
// NOOPT:  apply [[F2]]()
// CHECK: } // end sil function '$s4main28usePrespecializedEntryPointsyyF'
public func usePrespecializedEntryPoints() {
  publicPrespecialized2(SomeData())
  publicPrespecialized2(SomeOtherData())
}

// OPT: sil [available 50] [noinline] {{.*}}@$s22pre_specialized_module21publicPrespecialized2yyxlFAA8SomeDataV_Ts5 : $@convention(thin) (SomeData) -> ()
// OPT: sil [available 50] [noinline] {{.*}}@$s22pre_specialized_module21publicPrespecialized2yyxlF0a1_B8_module213SomeOtherDataV_Ts5 : $@convention(thin) (SomeOtherData) -> ()
