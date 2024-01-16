// RUN: %target-run-simple-swift(-parse-as-library -enable-experimental-feature Embedded -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop -Xlinker -dead_strip) | %FileCheck %s
// RUN: %target-run-simple-swift(-parse-as-library -O -enable-experimental-feature Embedded -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop -Xlinker -dead_strip) | %FileCheck %s
// RUN: %target-run-simple-swift(-parse-as-library -Osize -enable-experimental-feature Embedded -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop -Xlinker -dead_strip) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx

@main
struct Main {
  static func main() {
    var dict: [Int: StaticString] = [:]
    dict[11] = "hello"
    dict[33] = "!"
    dict[22] = "world"
    for key in dict.keys.sorted() {
      print(dict[key]!, terminator: " ")
    }
    print("")
  }
}

// CHECK: hello world !
