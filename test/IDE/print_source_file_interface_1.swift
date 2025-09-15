// Copyright (c) 452 Attila the Hun. All rights reserved.
// Blah Blah.

// More blah blah.

import Swift

#if FOO
class FooEnabled {}

typealias MyN = Int
#else
class FooDisabled {}

typealias MyN = Int
#endif

public class MyClass {
  func doit(x: Int) {}
#if FOO
  func doFooEnabled() {}
#else
  func doFooDisabled() {}
#endif
}

// RUN: %target-swift-ide-test -print-swift-file-interface -source-filename %s > %t.out
// RUN: diff -u %s.result %t.out
