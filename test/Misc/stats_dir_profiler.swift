// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/stats-events)
// RUN: %empty-directory(%t/stats-entities)
// RUN: %target-swiftc_driver -o %t/main -module-name main -stats-output-dir %t/stats-events %s -profile-stats-events
// RUN: %target-swiftc_driver -o %t/main -module-name main -stats-output-dir %t/stats-entities %s -profile-stats-entities
// RUN: %FileCheck -check-prefix=EVENTS -input-file %t/stats-events/*.dir/Time.User.events %s
// RUN: %FileCheck -check-prefix=ENTITIES -input-file %t/stats-entities/*.dir/Time.User.entities %s

// EVENTS: {{perform-sema;.*;typecheck-decl.* [0-9]+}}
// ENTITIES: {{perform-sema;.*;typecheck-fn bar\(\);typecheck-stmt.* [0-9]+}}

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
