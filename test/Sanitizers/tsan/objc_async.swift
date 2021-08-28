// RUN: %empty-directory(%t)
// RUN: %target-clang -fsanitize=thread %S/Inputs/objc_async.m -c -o %t/objc_async_objc.o
// RUN: %target-build-swift -sanitize=thread -O  -Xfrontend -disable-availability-checking -parse-as-library -module-name main -import-objc-header %S/Inputs/objc_async.h %s %t/objc_async_objc.o -o %t/objc_async
// RUN: %target-run %t/objc_async | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: objc_interop
// REQUIRES: tsan_runtime
// UNSUPPORTED: linux
// UNSUPPORTED: windows

// rdar://76038845
// UNSUPPORTED: use_os_stdlib

@main struct Main {
  static func main() async {
      let butt = Butt()
      let result = await butt.butt(1738)
      print("finishing \(result)")
  }
}

// CHECK: starting 1738
// CHECK-NEXT: finishing 679
