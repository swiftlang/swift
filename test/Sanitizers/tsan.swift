// This module is used by tsan_macos.swift.
// RUN: true

enum MyError : Error {
    case A
}

public func foobar(_ x: Int) throws {
  if x == 0 {
    throw MyError.A
  }
}

public func call_foobar() {
  do {
    try foobar(1)
  } catch(_) { }
}

// Test ThreadSanitizer execution end-to-end.

import Darwin

var threads: [pthread_t?] = []
var racey_x: Int;

for _ in 1...5 {
  var t : pthread_t?
  pthread_create(&t, nil, { _ in
    print("pthread ran")
    racey_x = 5;

    return nil
  }, nil)
  threads.append(t)
}

for t in threads {
  if t == nil {
    print("nil thread")
    continue
  }
  pthread_join(t!, nil)
}

