// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -o %t/main -module-name main -stats-output-dir %t %s -trace-stats-events
// RUN: %FileCheck -input-file %t/*.csv %s

// CHECK: {{[0-9]+,[0-9]+,"exit","lookup-direct","Sema.NominalTypeLookupDirectCount",[0-9]+,[0-9]+,"Proto","\[.*stats_dir_tracer.swift:13:1 - line:15:1\]"}}
// CHECK: {{[0-9]+,[0-9]+,"exit","typecheck-fn","Sema.NumTypesDeserialized",[0-9]+,[0-9]+,"foo\(\)","\[.*stats_dir_tracer.swift:9:8 - line:11:1\]"}}
// CHECK: {{[0-9]+,[0-9]+,"exit","typecheck-decl","Sema.NumConstraintScopes",[0-9]+,[0-9]+,"<pattern binding>","\[.*stats_dir_tracer.swift:23:5 - line:23:13\]"}}

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
