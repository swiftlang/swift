// RUN: %sourcekitd-test -req=cursor -pos=4:9 %s -- %s | %FileCheck %s

fileprivate let formatter: DateFormatter = {
    let formatter

// CHECK: s:24cursor_in_global_closureXefU_9formatterL_Xevp
