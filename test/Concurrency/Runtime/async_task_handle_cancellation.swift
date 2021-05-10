// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// This test is flaky on VS2017 (unknown reasons)
// UNSUPPORTED: MSVC_VER=15.0

// This test is failing on windows. SR-14447.
//
// UNSUPPORTED: OS=windows-msvc

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    let handle = detach {
      while (!Task.isCancelled) { // no need for await here, yay
        print("waiting")
      }

      print("done")
    }

    handle.cancel()

    // CHECK: done
    await handle.get()
  }
}
