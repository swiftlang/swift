// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library) | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: concurrency

@main struct Main {
  static func main() async {
    // FIXME: use Task.currentPriority once unsafeCurrent works
    let p = await Task.unsafeCurrentASYNC().task.priority
    assert(p == Task.Priority.default)
  }
}
