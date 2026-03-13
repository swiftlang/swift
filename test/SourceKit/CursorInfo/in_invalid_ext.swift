// Check that we don't crash.
extension Undefined {
  func foo() {
    guard let x
    // RUN: %sourcekitd-test -req=cursor -pos=%(line-1):15 %s -- %s | %FileCheck --check-prefix ONE %s
    // ONE: s:14in_invalid_ext3fooyXeXeF1xL_Xevp
  }
  func bar() {{
    guard let x
    // RUN: %sourcekitd-test -req=cursor -pos=%(line-1):15 %s -- %s | %FileCheck --check-prefix TWO %s
    // TWO: s:14in_invalid_ext3baryXeXeFXefU_1xL_Xevp
  }}
}
