// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-library -c %S/Inputs/testability_helper.swift -enable-testing -force-single-frontend-invocation -o %t/testability_helper.o -emit-module

// RUN: %target-build-swift %s -I %t -Xlinker %t/testability_helper.o -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

@testable import testability_helper

func log(_ value: Any, line: Int = #line) {
  print("\(line): \(value)")
}

log(Base(1)) // CHECK: {{^}}[[@LINE]]: instance 1{{$}}
log(Base(2).description) // CHECK: {{^}}[[@LINE]]: instance 2{{$}}
log((Base(3) as CustomStringConvertible).description) // CHECK: {{^}}[[@LINE]]: instance 3{{$}}

class Sub : Base {
  override var description: String {
    return "sub " + super.description
  }
}

log(Sub(1)) // CHECK: {{^}}[[@LINE]]: sub instance 1{{$}}
log(Sub(2) as Any as? Base) // CHECK: {{^}}[[@LINE]]: Optional(sub instance 2){{$}}

log(Base(1).callPrivate()) // CHECK: {{^}}[[@LINE]]: private 1{{$}}
log(Sub(2).callPrivate()) // CHECK: {{^}}[[@LINE]]: private 2{{$}}
log(getPrivateInstance().callPrivate()) // CHECK: {{^}}[[@LINE]]: really private{{$}}
