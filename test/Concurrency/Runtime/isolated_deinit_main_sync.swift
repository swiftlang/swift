// RUN: %target-run-simple-swift(-enable-experimental-feature IsolatedDeinit -Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

var isDead: Bool = false

public class Foo {
  @MainActor
  deinit {
    print("DEINIT")
    isDead = true
  }
}

func main() {
  print("isDead = \(isDead)")
  do {
    _ = Foo()
  }
  print("isDead = \(isDead)")
}

// CHECK: isDead = false
// CHECK: DEINIT
// CHECK: isDead = true
main()
