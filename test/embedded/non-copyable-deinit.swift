// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -O -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

var deinitCalled = false

struct S: ~Copyable {
  let s: String

  @inline(never)
  deinit {
    precondition(!deinitCalled)
    deinitCalled = true
    print(s)
  }
}

@main
struct Main {
  static func main() {
    // CHECK:      1
    _ = S(s: "1")
    // CHECK-NEXT: done
    print("done")
  }
}

