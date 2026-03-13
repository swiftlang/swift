// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/cse_metatype_conformanceA.swift -module-name moda -emit-module -emit-module-path %t/moda.swiftmodule
// RUN: %target-swift-frontend %S/Inputs/cse_metatype_conformanceB.swift -module-name modb -emit-module -emit-module-path %t/modb.swiftmodule -I %t
// RUN: %target-swift-frontend %S/Inputs/cse_metatype_conformanceC.swift -module-name modc -emit-module -emit-module-path %t/modc.swiftmodule -I %t
// RUN: %target-swift-frontend  -I %t -O -Xllvm -sil-print-types -emit-sil -sil-verify-all -parse-as-library %s | %FileCheck %s --check-prefix=CHECK

// Test CSE of init_existential_metatype. Combine instructions with
// conformance from the same module. Don't combine instructions with
// conformances from different modules.

// swift -frontend -emit-sil ./checkprotocoltype.swift -O -parse-as-library
import moda
import modb
import modc

@inline(never)
func callFoo(ptype: P.Type) {
  ptype.foo()
}

// CHECK-LABEL: sil @$s24cse_metatype_conformance15testConformanceyyF : $@convention(thin) () -> () {
// CHECK: [[MT:%.*]] = metatype $@thick A.Type
// CHECK: [[MTB:%.*]] = init_existential_metatype %0 : $@thick A.Type, $@thick any P.Type
// CHECK: [[F:%.*]] = function_ref @$s24cse_metatype_conformance7callFoo5ptypey4moda1P_pXp_tF : $@convention(thin) (@thick any P.Type) -> ()
// CHECK: apply [[F]]([[MTB]]) : $@convention(thin) (@thick any P.Type) -> ()
// CHECK: apply [[F]]([[MTB]]) : $@convention(thin) (@thick any P.Type) -> ()
// CHECK: [[MTC:%.*]] = init_existential_metatype %0 : $@thick A.Type, $@thick any P.Type
// CHECK: apply [[F]]([[MTC]]) : $@convention(thin) (@thick any P.Type) -> ()
// CHECK-LABEL: } // end sil function '$s24cse_metatype_conformance15testConformanceyyF'
public func testConformance() {
  let ptb1: P.Type = getPFromB()
  callFoo(ptype: ptb1)
  let ptb2: P.Type = getPFromB()
  callFoo(ptype: ptb2)
  let ptc: P.Type = getPFromC()
  callFoo(ptype: ptc)
}
