// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

// rdar://76038845
// UNSUPPORTED: use_os_stdlib

// https://bugs.swift.org/browse/SR-14333
// UNSUPPORTED: OS=windows-msvc

class X {
  init() {
    print("X: init")
  }
  deinit {
    print("X: deinit")
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func test_detach() async {
  for _ in 1...3 {
    let x = X()
    let h = detach {
      print("inside: \(x)")
    }
    await h.get()
  }
  // CHECK: X: init
  // CHECK: inside: main.X
  // CHECK: X: deinit
}


@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@main struct Main {
  static func main() async {
    await test_detach()
  }
}
