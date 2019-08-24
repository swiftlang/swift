/*
Copyright (c) 452 Attila the Hun. All rights reserved.
*/
/* Blah Blah */

/* More Blah Blah */

public class MyClass {
  func doit(x: Int) {}
}

/// This is a very nice extension.
extension MyClass {

  /// and a nice subscript.
  subscript(i: Int) -> Int { return 0 }
}

// RUN: %target-swift-ide-test -print-swift-file-interface -source-filename %s > %t.out
// RUN: diff -u %s.result %t.out
