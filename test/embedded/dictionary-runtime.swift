// RUN: %target-run-simple-swift(-parse-as-library -enable-experimental-feature Embedded -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop -Xlinker -dead_strip) | %FileCheck %s
// RUN: %target-run-simple-swift(-parse-as-library -O -enable-experimental-feature Embedded -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop -Xlinker -dead_strip) | %FileCheck %s
// RUN: %target-run-simple-swift(-parse-as-library -Osize -enable-experimental-feature Embedded -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop -Xlinker -dead_strip) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

var dict1: [Int: Int] = [1:10]
var dict2: [Int: Int] = [1:20]

func do_swap(n: Int) {
  swap(&dict2[n], &dict1[n])
}


@main
struct Main {
  static func main() {
    var dict: [Int: StaticString] = [:]
    dict[11] = "hello"
    dict[33] = "!"
    dict[22] = "world"

    // CHECK: hello world !
    for key in dict.keys.sorted() {
      print(dict[key]!, terminator: " ")
    }
    print("")

    do_swap(n: 1)
    // CHECK: 20
    print(dict1[1]!)
    // CHECK: 10
    print(dict2[1]!)
  }
}

