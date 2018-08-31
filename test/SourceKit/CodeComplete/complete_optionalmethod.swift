@objc protocol Proto {
  @objc optional func optionalMethod() -> Int
}

func test<T : Proto>(obj: T) {
  let _ = obj.
}

// RUN: %sourcekitd-test -req=complete -pos=6:15 %s -- %s > %t.response
// RUN: diff -u %s.response %t.response
// REQUIRES: objc_interop
