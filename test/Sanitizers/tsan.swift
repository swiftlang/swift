// RUN: %target-swiftc_driver %s -g -sanitize=thread -o %t_tsan-binary
// RUN: not env TSAN_OPTIONS=abort_on_error=0 %target-run %t_tsan-binary 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: CPU=x86_64
// REQUIRES: tsan_runtime
// XFAIL: linux

// Test ThreadSanitizer execution end-to-end.

import Darwin
var user_interactive_thread: pthread_t?
var user_interactive_thread2: pthread_t?
var racey_x: Int;

pthread_create(&user_interactive_thread, nil, { _ in
    print("pthread ran")
    racey_x = 5;

    return nil
}, nil)

pthread_create(&user_interactive_thread2, nil, { _ in
    print("pthread2 ran")
    racey_x = 6;

    return nil
}, nil)

pthread_join(user_interactive_thread!, nil)
pthread_join(user_interactive_thread2!, nil)

// CHECK: ThreadSanitizer: data race
