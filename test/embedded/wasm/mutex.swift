// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: OS=wasip1
// REQUIRES: swift_feature_Embedded

import Synchronization
@main struct Main {
  static func main() {
    let m = Mutex(42)

    m.withLock {
      print("Hello \($0)")  // CHECK: Hello 42
      $0 = 37
    }
    
    m.withLock {
      print("Hello \($0)")  // CHECK: Hello 37
    }
  }
}
