// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -o %t/main -module-name main -stats-output-dir %t %s -trace-stats-events
// RUN: %FileCheck -input-file %t/*.csv %s

// CHECK-DAG: {{[0-9]+,[0-9]+,"exit","typecheck-expr","Sema.NumConstraintScopes",[0-9]+,[0-9]+,"Sequence","\[.*stats_dir_tracer.swift.*\]"}}
// CHECK-DAG: {{[0-9]+,[0-9]+,"exit","SuperclassDeclRequest","Sema.SuperclassDeclRequest",[0-9]+,[0-9]+,"Bar","\[.*stats_dir_tracer.swift.*\]"}}

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
