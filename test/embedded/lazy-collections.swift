// RUN: %target-run-simple-swift(-parse-as-library -enable-experimental-feature Embedded -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu

@main
struct Main {
  static func main() {
    [1, 2, 3].lazy.filter { $0 % 2 == 0 }.forEach { print($0, terminator: " ") }
    print("")
    // CHECK: 2

    [1, 2, 3].lazy.map { $0 * 2 }.forEach { print($0, terminator: " ") }
    print("")
    // CHECK: 2 4 6
  }
}
