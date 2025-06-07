// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo %embedded-unicode-tables) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

func f<T: LosslessStringConvertible>(t: inout T?, s: String) {
  t = .init(s)
}

@main
struct Main {
  static func main() {
    if let b = Bool.init("true"), b == true { print("OK 1") } // CHECK: OK 1
    var b: Bool?
    f(t: &b, s: "false")
    if let b, b == false { print("OK 2") } // CHECK: OK 2

    if let i = Int.init("17"), i == 17 { print("OK 3") } // CHECK: OK 3
    var i: Int?
    f(t: &i, s: "777")
    if let i, i == 777 { print("OK 4") } // CHECK: OK 4

    // TODO: Add float parsing to Embedded Swift that does not rely on libc
    // if let fl = Float.init("42.42"), fl == 42.42 { print("OK 5") } // XXX: OK 5
    // var fl: Float?
    // f(t: &fl, s: "12.34")
    // if let fl, fl == 12.34 { print("OK 6") } // XXX: OK 6
  }
}
