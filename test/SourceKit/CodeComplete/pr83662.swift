// Make sure we don't crash
struct S {
  init?() {
    return nil
    // RUN: %sourcekitd-test -req=complete.open -pos=%(line-1):12 %s -- %s
  }
}
