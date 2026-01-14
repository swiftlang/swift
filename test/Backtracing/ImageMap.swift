// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Xfrontend -disable-availability-checking -Onone -o %t/ImageMap.exe
// RUN: %target-codesign %t/ImageMap.exe
// RUN: %target-run %t/ImageMap.exe | %FileCheck --ignore-case %s

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx || OS=linux-gnu || OS=windows-msvc

import Runtime

@main
struct ImageMapTest {
  static func main() {
    let map = ImageMap.capture()

    // We expect ImageMap, followed by one or more additional lines

    // CHECK: {{0x[0-9a-f]*-0x[0-9a-f]*}} {{(<no build ID>|[0-9a-f]*)}} ImageMap.exe {{.*[\\/]}}ImageMap.exe
    // CHECK-NEXT: {{0x[0-9a-f]*-0x[0-9a-f]*}} {{(<no build ID>|[0-9a-f]*)}} [[NAME:[^ ]*]] {{.*[\\/]}}[[NAME]]
    print(map)
  }
}


