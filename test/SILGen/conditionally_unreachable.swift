// RUN: %target-swift-emit-silgen -enable-sil-ownership -parse-stdlib -primary-file %s | %FileCheck %s -check-prefix=RAW
// RUN: %target-swift-emit-sil -enable-sil-ownership -assert-config Debug -parse-stdlib -primary-file %s | %FileCheck -check-prefix=DEBUG %s
// RUN: %target-swift-emit-sil -enable-sil-ownership -O -assert-config Debug -parse-stdlib -primary-file %s | %FileCheck -check-prefix=DEBUG %s
// RUN: %target-swift-emit-sil -enable-sil-ownership -assert-config Release -parse-stdlib -primary-file %s | %FileCheck -check-prefix=RELEASE %s
// RUN: %target-swift-emit-sil -enable-sil-ownership -O -assert-config Release -parse-stdlib -primary-file %s | %FileCheck -check-prefix=RELEASE %s

import Swift

@_silgen_name("foo") func foo()

func condUnreachable() {
  if Int32(Builtin.assert_configuration()) == 0 {
    foo()
  } else {
    Builtin.conditionallyUnreachable()
  }
}

// RAW-LABEL: sil hidden @$s25conditionally_unreachable15condUnreachableyyF 
// RAW:         cond_br {{%.*}}, [[YEA:bb[0-9]+]], [[NAY:bb[0-9]+]]
// RAW:       [[YEA]]:
// RAW:         function_ref @foo
// RAW:       [[NAY]]:
// RAW:         builtin "conditionallyUnreachable"

// DEBUG-LABEL: sil hidden @$s25conditionally_unreachable15condUnreachableyyF 
// DEBUG-NOT:     cond_br
// DEBUG:         function_ref @foo
// DEBUG-NOT:     {{ unreachable}}
// DEBUG:         return

// RELEASE-LABEL: sil hidden @$s25conditionally_unreachable15condUnreachableyyF 
// RELEASE-NOT:     cond_br
// RELEASE-NOT:     function_ref @foo
// RELEASE-NOT:     return
// RELEASE-NOT:     builtin
// RELEASE:         {{ unreachable}}
