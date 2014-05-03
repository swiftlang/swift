// RUN: %swift -emit-silgen -parse-stdlib %s | FileCheck %s -check-prefix=RAW
// RUN: %swift -emit-sil -AssertConfig=Debug -parse-stdlib %s | FileCheck -check-prefix=DEBUG %s
// RUN: %swift -emit-sil -O -AssertConfig=Debug -parse-stdlib %s | FileCheck -check-prefix=DEBUG %s
// RUN: %swift -emit-sil -AssertConfig=Release -parse-stdlib %s | FileCheck -check-prefix=RELEASE %s
// RUN: %swift -emit-sil -O -AssertConfig=Release -parse-stdlib %s | FileCheck -check-prefix=RELEASE %s

import Swift

@asmname("foo") func foo()

func condUnreachable() {
  if Int32(Builtin.assert_configuration()) == 0 {
    foo()
  } else {
    Builtin.conditionallyUnreachable()
  }
}

// RAW-LABEL: sil @_TF25conditionally_unreachable15condUnreachableFT_T_ 
// RAW:         cond_br {{%.*}}, [[YEA:bb[0-9]+]], [[NAY:bb[0-9]+]]
// RAW:       [[YEA]]:
// RAW:         function_ref @foo
// RAW:       [[NAY]]:
// RAW:         builtin_function_ref "conditionallyUnreachable"

// DEBUG-LABEL: sil @_TF25conditionally_unreachable15condUnreachableFT_T_ 
// DEBUG-NOT:     cond_br
// DEBUG:         function_ref @foo
// DEBUG-NOT:     unreachable
// DEBUG:         return

// RELEASE-LABEL: sil @_TF25conditionally_unreachable15condUnreachableFT_T_ 
// RELEASE-NOT:     cond_br
// RELEASE-NOT:     function_ref @foo
// RELEASE-NOT:     return
// RELEASE-NOT:     builtin_function_ref
// RELEASE:         unreachable
