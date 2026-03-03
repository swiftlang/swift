// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift

// -playground
// RUN: not %target-typecheck-verify-swift -swift-version 5 -playground %t/main.swift
// RUN: not %target-typecheck-verify-swift -swift-version 6 -playground %t/main.swift

// -pc-macro -playground
// RUN: not %target-typecheck-verify-swift -swift-version 5 -playground -pc-macro %t/main.swift
// RUN: not %target-typecheck-verify-swift -swift-version 6 -playground -pc-macro %t/main.swift

for x in y {
  x
}
