// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Onone -g -o %t/CrashWithThreads
// RUN: %target-codesign %t/CrashWithThreads
// RUN: not --crash env SWIFT_BACKTRACE=enable=yes,cache=no,swift-backtrace=%backtracer %target-run %t/CrashWithThreads 2>&1 | %FileCheck -vv %s -dump-input-filter=all
// RUN: not --crash env SWIFT_BACKTRACE=enable=yes,cache=no,swift-backtrace=%backtracer,threads=all %target-run %t/CrashWithThreads 2>&1 | %FileCheck -vv %s -dump-input-filter=all

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: asan
// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx

import Darwin

let mutex = UnsafeMutablePointer<pthread_mutex_t>.allocate(capacity: 1)
guard unsafe pthread_mutex_init(mutex, nil) == 0 else {
  fatalError("pthread_mutex_init failed")
}

func reallyCrashMe() {
  print("I'm going to crash now")
  let ptr = UnsafeMutablePointer<Int>(bitPattern: 4)!
  ptr.pointee = 42
}

func crashMe() {
  reallyCrashMe()
}

func lockMutex() {
  guard unsafe pthread_mutex_lock(mutex) == 0 else {
    fatalError("pthread_mutex_lock failed")
  }
}

func unlockMutex() {
  guard unsafe pthread_mutex_unlock(mutex) == 0 else {
    fatalError("pthread_mutex_unlock failed")
  }
}


func spawnThread(_ shouldCrash: Bool) {
  var thread = pthread_t(nil)
  if shouldCrash {
    pthread_create(&thread, nil, { _ in
                                // take mutex
                                lockMutex()

                                crashMe()

                                // this should not be run
                                unlockMutex()

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

let crashingThreadIndex = (1..<4).randomElement()

lockMutex()

// hold a mutex
for threadIndex in 1..<4 {
  spawnThread(threadIndex == crashingThreadIndex)
}

// release mutex
unlockMutex()

while (true) {
  sleep(10)
}

// CHECK: *** Program crashed: Bad pointer dereference at 0x{{0+}}4 ***

// make sure there are no threads before the crashing thread (rdar://164566321)
// and check that we have some vaguely sane and symbolicated threads, including
// a main thread (AFTER the crashed thread) and at least one other thread

// we expect the first thread not to be another thread, it should be the crashing thread instead
// CHECK-NOT: Thread {{[0-9]+( ".*")?}}:
// CHECK: Thread {{[0-9]+}} {{(".*" )?}}crashed:
// CHECK: {{0x[0-9a-f]+}} reallyCrashMe()

// CHECK: Thread {{[0-9]*( ".*")?}}:
// CHECK: {{0x[0-9a-f]+.*main.* CrashWithThreads}}

// CHECK: Thread {{[0-9]*( ".*")?}}:
