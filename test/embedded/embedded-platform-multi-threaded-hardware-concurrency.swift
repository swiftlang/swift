// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Embedded -wmo -import-bridging-header %t/BridgingHeader.h -Xcc -I%S/../../stdlib/public/EmbeddedPlatform %t/Main.swift -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o %target-embedded-multi-threaded-posix-shim %target-embedded-posix-shim -o %t/posix.out -dead_strip -pthreads
// RUN: %if OS=wasip1 %{ env -u %env-SWIFT_HARDWARE_CONCURRENCY %target-run %t/posix.out | %FileCheck %s --check-prefix=ONE %} %else %{ env -u %env-SWIFT_HARDWARE_CONCURRENCY %target-run %t/posix.out | %FileCheck %s --check-prefix=SYSCONF %}
// RUN: %if OS=wasip1 %{ env %env-SWIFT_HARDWARE_CONCURRENCY=999 %target-run %t/posix.out | %FileCheck %s --check-prefix=SET %} %else %{ env %env-SWIFT_HARDWARE_CONCURRENCY=999 %target-run %t/posix.out | %FileCheck %s --check-prefix=SYSCONF %}
// RUN: %if OS=wasip1 %{ env %env-SWIFT_HARDWARE_CONCURRENCY=2147483647 %target-run %t/posix.out | %FileCheck %s --check-prefix=MAX %}
// RUN: %if OS=wasip1 %{ env %env-SWIFT_HARDWARE_CONCURRENCY=0 %target-run %t/posix.out | %FileCheck %s --check-prefix=ONE %}
// RUN: %if OS=wasip1 %{ env %env-SWIFT_HARDWARE_CONCURRENCY= %target-run %t/posix.out | %FileCheck %s --check-prefix=ONE %}
// RUN: %if OS=wasip1 %{ env %env-SWIFT_HARDWARE_CONCURRENCY=abc %target-run %t/posix.out | %FileCheck %s --check-prefix=ONE %}
// RUN: %if OS=wasip1 %{ env %env-SWIFT_HARDWARE_CONCURRENCY=4x %target-run %t/posix.out | %FileCheck %s --check-prefix=ONE %}
// RUN: %if OS=wasip1 %{ env %env-SWIFT_HARDWARE_CONCURRENCY=+4 %target-run %t/posix.out | %FileCheck %s --check-prefix=ONE %}
// RUN: %if OS=wasip1 %{ env %env-SWIFT_HARDWARE_CONCURRENCY=2147483648 %target-run %t/posix.out | %FileCheck %s --check-prefix=ONE %}

// RUN: %if OS=macosx %{ %target-embedded-link %target-clang-resource-dir-opt %t/main.o %target-embedded-multi-threaded-darwin-shim %target-embedded-posix-shim -o %t/darwin.out -dead_strip %}
// RUN: %if OS=macosx %{ env -u %env-SWIFT_HARDWARE_CONCURRENCY %target-run %t/darwin.out | %FileCheck %s --check-prefix=SYSCONF %}
// RUN: %if OS=macosx %{ env %env-SWIFT_HARDWARE_CONCURRENCY=999 %target-run %t/darwin.out | %FileCheck %s --check-prefix=SYSCONF %}

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu || wasi_threads
// REQUIRES: swift_feature_Embedded

// BEGIN BridgingHeader.h

#include <unistd.h>
#include <swift/EmbeddedPlatform.h>

// BEGIN Main.swift

@main
struct Main {
  static func main() {
    let count = _swift_thread_getHardwareConcurrency()
    print("count: \(count)")
    // ONE: count: 1{{$}}
    // SET: count: 999{{$}}
    // MAX: count: 2147483647{{$}}
    print("equals sysconf: \(count == sysconf(Int32(_SC_NPROCESSORS_ONLN)))")
    // SYSCONF: equals sysconf: true
  }
}
