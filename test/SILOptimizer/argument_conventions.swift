// RUN: %target-swift-frontend -emit-sil %s \
// RUN:   -sil-verify-all \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   2>&1 | %FileCheck %s

import Builtin

class C {}

@_nonescapable
struct NE {}

// Verify that SILFunction_getSelfArgumentIndex respects SILFunctionConventions
// and returns right value even in the presence of indirect results.
//
// CHECK: [0]  indirect result: indirectOut
// CHECK: [1]        parameter: directGuaranteed
// CHECK-LABEL: end running test 1 of {{[^,]+}} on indirect_result: argument_conventions
sil [ossa] @indirect_result : $@convention(method) <T> (@guaranteed C) -> @out T {
entry(%result : $*T, %instance : @guaranteed $C):
    specify_test "argument_conventions"
    unreachable
}


sil [ossa] @result_dependson_copy : $@convention(method) (@guaranteed C) -> _inherit(1) @out NE {
entry(%result : $*NE, %instance : @guaranteed $C):
    specify_test "argument_conventions"
    unreachable
}
