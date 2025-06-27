// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -module-name Test -debug-constraints -blocklist-file %t/blocklist.yaml -verify %t/main.swift 2>%t.err
// RUN: %FileCheck %t/main.swift < %t.err

//--- blocklist.yaml
---
ShouldUseTypeCheckerPerfHacks:
  ModuleName:
    - Test

//--- main.swift
_ = 1 + 2 + 3

// CHECK: [favored] {{.*}} bound to decl Swift.(file).Int extension.+ : (Int.Type) -> (Int, Int) -> Int
