// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -D DEFINITIONS_ONLY -module-name adcs_same_case_source %S/../../validation-test/closure_specialization/adcs_same_case_source.swift -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.before
// RUN: %FileCheck %S/../../validation-test/closure_specialization/adcs_same_case_source.swift < %t.sil
// RUN: %FileCheck %s --check-prefix=BEFORE < %t.before
// RUN: %FileCheck %s --check-prefix=LATE < %t.before

// Use the actual source so the synthesized one-element tuple payloads retain
// their AST types. The first ADCS input has distinct tuple instructions saving
// the same callback for the same branch-tracing case. The validation pair also
// checks that the optimized VJP returns the specialized pullback and that all
// opaque callback applications disappear from its body.

// BEFORE-LABEL: // reverse-mode derivative of twoSelections
// BEFORE: bb3:
// BEFORE: %[[FIRST:[0-9]+]] = tuple (%[[CALLBACK:[0-9]+]])
// BEFORE-NEXT: %{{[0-9]+}} = enum [[ENUMTYPE:\$_AD__[^, ]+]], [[ENUMCASE:#[^ ]+]], %[[FIRST]]
// BEFORE-NOT: } // end sil function
// BEFORE: bb4:
// BEFORE: %[[SECOND:[0-9]+]] = tuple (%[[CALLBACK]])
// BEFORE-NEXT: %{{[0-9]+}} = enum [[ENUMTYPE]], [[ENUMCASE]], %[[SECOND]]
// BEFORE-NOT: } // end sil function
// BEFORE: return
// BEFORE-NEXT: } // end sil function

// Mid-level sinking creates different capture-tuple SSA values inside two
// constructors of one specialized bound case. Keep the existing pullback.
// LATE: stage MidLevel,Function
// LATE: // reverse-mode derivative of squaredSelection(_:_:_:)
// LATE: bb0(%[[X:[0-9]+]] : $Float,
// LATE: bb3:
// LATE-NEXT: %[[LEFT_CAPTURE:[0-9]+]] = tuple (%[[X]], %[[X]])
// LATE-NEXT: %[[LEFT:[0-9]+]] = tuple (%[[LEFT_CAPTURE]])
// LATE-NEXT: %{{[0-9]+}} = enum [[TYPE:\$_AD__[^, ]+]], [[CASE:#[^ ]+]], %[[LEFT]]
// LATE: bb4:
// LATE-NEXT: %[[RIGHT_CAPTURE:[0-9]+]] = tuple (%[[X]], %[[X]])
// LATE-NEXT: %[[RIGHT:[0-9]+]] = tuple (%[[RIGHT_CAPTURE]])
// LATE-NEXT: %{{[0-9]+}} = enum [[TYPE]], [[CASE]], %[[RIGHT]]
// LATE: function_ref @[[PB:[^ ]+]]
// LATE: return
// LATE-NEXT: } // end sil function
// LATE-NOT: *** New SIL function
// LATE: *** SIL function after {{.*}}stage MidLevel,Function
// LATE-NEXT: // reverse-mode derivative of squaredSelection(_:_:_:)
// LATE: function_ref @[[PB]]
