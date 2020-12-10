// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/objc_async.m -c -o %t/objc_async_objc.o
// RUN: %target-build-swift -Xfrontend -enable-experimental-concurrency %import-libdispatch -import-objc-header %S/Inputs/objc_async.h %s %t/objc_async_objc.o -o %t/objc_async
// RUN: %target-run %t/objc_async | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: objc_interop

import _Concurrency

let task = Task.runDetached {
  let butt = Butt()
  let result = await butt.butt(1738)
  print("finishing \(result)")
  // FIXME: with a proper task runtime, we should exit naturally once all tasks
  // complete
  exit(0)
}

// FIXME: use the real task runtime facilities
task.run()

dispatchMain()

// CHECK: starting 1738
// CHECK-NEXT: finishing 679
