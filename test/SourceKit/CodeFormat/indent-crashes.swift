subscript (__PLACEHOLDER_0__) -> __PLACEHOLDER_1__ {
  get {
     __PLACEHOLDER_2__
  }
  set(__PLACEHOLDER_3__) {
     __PLACEHOLDER_4__
  }
}

// RUN: %sourcekitd-test -req=format -line=1 -length=1 %s >%t.response
