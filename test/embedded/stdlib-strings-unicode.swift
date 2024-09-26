// RUN: %target-run-simple-swift(                            -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xlinker %swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos/libswiftUnicodeDataTables.a) | %FileCheck %s
// RUN: %target-run-simple-swift(-Osize -Xlinker -dead_strip -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xlinker %swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos/libswiftUnicodeDataTables.a) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

@main
struct Main {
  static func main() {
    let str = "Hello😊"
    print(str) // CHECK: Hello😊
    print(str.dropLast()) // CHECK: Hello
    print(str.dropLast().count) // CHECK: 5

    var dict: [String:String] = [:]
    let c = "Cafe\u{301}"
    let d = "Cafe\u{301}"
    let e = "Café"
    let f = "Caf\u{65}\u{301}"
    let g = "Caf\u{e9}"
    dict[c] = str
    dict[d] = str
    dict[e] = str
    dict[f] = str
    dict[g] = str
    print(dict.count) // CHECK: 1
    print(dict[f]!) // CHECK: Hello😊

    var emoji = ""
    // VAMPIRE, ZERO-WIDTH JOINER, FEMALE SIGN, VARIATION SELECTOR-16
    emoji += "\u{1f9db}"
    emoji += "\u{200d}"
    emoji += "\u{2640}"
    emoji += "\u{fe0f}"
    print(emoji.count) // CHECK: 1
  }
}
