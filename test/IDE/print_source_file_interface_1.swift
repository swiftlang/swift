// Copyright (c) 452 Attila the Hun. All rights reserved.
// Blah Blah.

// More blah blah.

public class MyClass {
  func doit(x: Int) {}
}

// RUN: %target-swift-ide-test -print-swift-file-interface -source-filename %s > %t.out
// RUN: diff -u %s.result %t.out
