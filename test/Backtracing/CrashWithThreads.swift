// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Onone -g -o %t/CrashWithThreads
// RUN: %target-codesign %t/CrashWithThreads
// RUN: (env SWIFT_BACKTRACE=enable=yes,cache=no,swift-backtrace=%backtracer %target-run %t/CrashWithThreads 2>&1 || true) | %FileCheck -vv %s -dump-input-filter=all

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: asan
// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx

#if canImport(Darwin)
  import Darwin
#elseif canImport(Glibc)
  import Glibc
#elseif canImport(Android)
  import Android
#elseif os(Windows)
  import CRT
#else
#error("Unsupported platform")
#endif

func reallyCrashMe() {
  print("I'm going to crash now")
  let ptr = UnsafeMutablePointer<Int>(bitPattern: 4)!
  ptr.pointee = 42
}

func crashMe() {
  reallyCrashMe()
}

func spawnThread(_ shouldCrash: Bool) {
  #if os(Linux)
  var thread: pthread_t = 0
  #elseif os(macOS)
  var thread = pthread_t(nil)
  #endif
  if shouldCrash {
    pthread_create(&thread, nil, { _ in
                                crashMe()
                                // this should not be run
                                while (true) {
                                    sleep(10)
                                  }
                              }, nil)
  } else {
    pthread_create(&thread, nil, { _ in
                                  while (true) {
                                    sleep(10)
                                  }
                                }, nil)
  }
}

let crashingThreadIndex = (1..<10).randomElement()

for threadIndex in 1..<10 {
  spawnThread(threadIndex == crashingThreadIndex)
}

while (true) {
  sleep(10)
}

// CHECK: *** Program crashed: Bad pointer dereference at 0x{{0+}}4 ***

// make sure there are no threads before the crashing thread (rdar://164566321)

// we expect the first thread not to be thread 0, it should be the crashing thread instead
// CHECK-NOT: Thread 0{{( ".*")?}}:

// we expect a crash on a thread other than 0
// CHECK: Thread {{[1-9][0-9]*}} {{(".*" )?}}crashed:

// CHECK: 0                    0x{{[0-9a-f]+}} reallyCrashMe() + {{[0-9]+}} in CrashWithThreads at {{.*}}/CrashWithThreads
// CHECK-NEXT: 1 [ra]          0x{{[0-9a-f]+}} crashMe() + {{[0-9]+}} in CrashWithThreads at {{.*}}/CrashWithThreads
// CHECK-NEXT: 2 [ra]          0x{{[0-9a-f]+}} closure #{{[0-9]}} in spawnThread(_:) + {{[0-9]+}} in CrashWithThreads at {{.*}}/CrashWithThreads
// CHECK-NEXT: 3 [ra] [thunk]  0x{{[0-9a-f]+}} @objc closure #{{[0-9]}} in spawnThread(_:) + {{[0-9]+}} in CrashWithThreads at {{.*}}<compiler-generated>

// CHECK: Registers:

// CHECK: Images ({{[0-9]+}} omitted):

// CHECK: {{0x[0-9a-f]+}}–{{0x[0-9a-f]+}}{{ +}}{{([0-9a-f]+|<no build ID>)}}{{ +}}CrashWithThreads{{ +}}{{.*}}/CrashWithThreads
