// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu || OS=wasip1
// REQUIRES: swift_feature_Embedded

class BaseError: Error {
  let code: Int
  init(code: Int) { self.code = code }
  deinit { print("BaseError deinit") }
}

class SubError: BaseError {
  let detail: Int
  init(code: Int, detail: Int) {
    self.detail = detail
    super.init(code: code)
  }
  deinit { print("SubError deinit") }
}

func throwSub() throws {
  throw SubError(code: 1, detail: 42)
}

@main
struct Main {
  static func main() {
    // Catch subclass as base class
    do {
      try throwSub()
    } catch let e as BaseError {
      print(e.code == 1 ? "OK1" : "FAIL1")
    } catch {
      print("FAIL-unexpected")
    }

    // Catch subclass as exact subclass type
    do {
      try throwSub()
    } catch let e as SubError {
      print(e.detail == 42 ? "OK2" : "FAIL2")
    } catch {
      print("FAIL-unexpected")
    }
  }
}

// CHECK: OK1
// CHECK: SubError deinit
// CHECK: BaseError deinit
// CHECK: OK2
// CHECK: SubError deinit
// CHECK: BaseError deinit
// CHECK-NOT: FAIL
// CHECK-NOT: deinit
