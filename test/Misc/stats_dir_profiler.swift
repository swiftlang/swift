// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/stats-events)
// RUN: %empty-directory(%t/stats-entities)
// RUN: %target-swiftc_driver -o %t/main -module-name main -stats-output-dir %t/stats-events %s -profile-stats-events -Xfrontend -fine-grained-timers
// RUN: %target-swiftc_driver -o %t/main -module-name main -stats-output-dir %t/stats-entities %s -profile-stats-entities -Xfrontend -fine-grained-timers

// RUN: %FileCheck -check-prefix=EVENTS -input-file %long-tmp/stats-events/*.dir/Time.User.events %s
// RUN: %FileCheck -check-prefix=ENTITIES -input-file %long-tmp/stats-entities/*.dir/Time.User.entities %s

// It's good enough to run the test on macOS and linux.
// It takes very long on Windows because the * may expand to a path longer than 260 characters.

// REQUIRES: OS=macosx || OS=linux-gnu

// EVENTS: {{perform-sema;.*;typecheck-decl.* [0-9]+}}
// ENTITIES: {{perform-sema;.*;TypeCheckFunctionBodyRequest bar\(\);typecheck-stmt.* [0-9]+}}

public func foo() {
    print("hello")
}

protocol Proto {
  func bar() -> Int;
}

class Bar {
  typealias T = Int
}

extension Bar : Proto {
  func bar() -> T {
    let x = 1
    let y = Int(1.0)
    return x + y
  }
}
