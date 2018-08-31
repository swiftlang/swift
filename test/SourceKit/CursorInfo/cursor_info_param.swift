func simpleParams(a: Int, b theB: Int) {}

struct AccessorTest {
  subscript(a: Int, b theB: Int) -> Int { return a }
  var prop: Int {
    get { return 0 }
    set(v) {}
  }
}

#sourceLocation(file: "custom.swuft", line: 2000)
func customSourceLocation(a: Int) {}
#sourceLocation()

// RUN: %sourcekitd-test -req=cursor -pos=1:19 %s -- %s | %FileCheck -check-prefix=CHECK-FUNC-A %s
// CHECK-FUNC-A: s:17cursor_info_param12simpleParams1a1bySi_SitFACL_Sivp
// CHECK-FUNC-A: PARENT OFFSET: 5

// RUN: %sourcekitd-test -req=cursor -pos=1:27 %s -- %s | %FileCheck -check-prefix=CHECK-FUNC-B %s
// CHECK-FUNC-B: s:17cursor_info_param12simpleParams1a1bySi_SitF{{$}}

// RUN: %sourcekitd-test -req=cursor -pos=1:29 %s -- %s | %FileCheck -check-prefix=CHECK-FUNC-THEB %s
// CHECK-FUNC-THEB: s:17cursor_info_param12simpleParams1a1bySi_SitF4theBL_Sivp
// CHECK-FUNC-THEB-NOT: PARENT OFFSET

// RUN: %sourcekitd-test -req=cursor -pos=4:13 %s -- %s | %FileCheck -check-prefix=CHECK-SUBSCRIPT-A %s
// FIXME: This USR is wrong; see https://bugs.swift.org/browse/SR-8660.
// CHECK-SUBSCRIPT-A: s:17cursor_info_param12AccessorTestV1aL_Sivp
// CHECK-SUBSCRIPT-A: PARENT OFFSET: 67

// RUN: %sourcekitd-test -req=cursor -pos=4:21 %s -- %s | %FileCheck -check-prefix=CHECK-SUBSCRIPT-B %s
// CHECK-SUBSCRIPT-B: s:17cursor_info_param12AccessorTestV

// RUN: %sourcekitd-test -req=cursor -pos=4:23 %s -- %s | %FileCheck -check-prefix=CHECK-SUBSCRIPT-THEB %s
// FIXME: This USR is wrong; see https://bugs.swift.org/browse/SR-8660.
// CHECK-SUBSCRIPT-THEB: s:17cursor_info_param12AccessorTestV4theBL_Sivp
// CHECK-SUBSCRIPT-THEB-NOT: PARENT OFFSET

// RUN: %sourcekitd-test -req=cursor -pos=7:9 %s -- %s | %FileCheck -check-prefix=CHECK-SETTER-V %s
// CHECK-SETTER-V: s:17cursor_info_param12AccessorTestV4propSivs1vL_Sivp
// CHECK-SETTER-V: PARENT OFFSET: 161

// RUN: %sourcekitd-test -req=cursor -pos=12:27 %s -- %s | %FileCheck -check-prefix=CHECK-CUSTOM-SOURCELOCATION %s
// CHECK-CUSTOM-SOURCELOCATION: s:17cursor_info_param20customSourceLocation1aySi_tFACL_Sivp
// CHECK-CUSTOM-SOURCELOCATION: PARENT OFFSET: 233
