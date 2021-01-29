// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library) | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: concurrency

@main struct Main {
  static func main() async {
    let p = await Task.currentPriority()
    // CHECK: priority: default
    print("priority: \(p)")
    assert(p == Task.Priority.default)
  }
}
