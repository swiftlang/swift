// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library) | %FileCheck %s --dump-input=always
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64

import Dispatch
#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

@main struct Main {
  static func main() async {
    let one = await Task.__unsafeCurrentAsync().task // FIXME: replace with Task.current
    let two = await Task.__unsafeCurrentAsync().task // FIXME: replace with Task.current
    print(one == two) // CHECK: true
    print("hashes equal: \(one.hashValue == two.hashValue)") // CHECK: hashes equal: true

    async let x = Task.__unsafeCurrentAsync().task // FIXME: replace with Task.current

    let three = await x
    print(three == two) // CHECK: false
    print("hashes equal: \(three.hashValue == two.hashValue)") // CHECK: hashes equal: false
  }
}
