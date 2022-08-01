// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library -emit-library -emit-module-path %t/type_wrapper_defs.swiftmodule -module-name type_wrapper_defs %S/Inputs/type_wrapper_defs.swift -o %t/%target-library-name(type_wrapper_defs)
// RUN: %target-build-swift -ltype_wrapper_defs -module-name main -I %t -L %t %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

import type_wrapper_defs

var p: Person<String> = .init(name: "P", projects: ["A", "B"])
// CHECK: Wrapper.init($Storage(name: "P", projects: ["A", "B"]))

print(p.name)
// CHECK: in getter
// CHECK-NEXT: P
print(p.projects)
// CHECK: in getter
// CHECK-NEXT: ["A", "B"]

p.name = "OtherP"
// CHECK: in setter => OtherP
p.projects.append("C")
// CHECK: in getter
// CHECK-NEXT: in setter => ["A", "B", "C"]


func addProjects<T>(p: inout Person<T>, _ newProjects: [T]) {
  p.projects.append(contentsOf: newProjects)
}

addProjects(p: &p, ["D"])
// CHECK: in getter
// CHECK: in setter => ["A", "B", "C", "D"]

print(p.name)
// CHECK: in getter
// CHECK-NEXT: OtherP

print(p.projects)
// CHECK: in getter
// CHECK-NEXT: ["A", "B", "C", "D"]
