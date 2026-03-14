class Items {
  func test() {
    // Should not crash
    // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):9 %s -- %s
    _ = Invalid.sink { [weak self] items in
    }
  }
}
