// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency

@main struct Main {
  static func main() async {
    let handle = Task.runDetached {
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
