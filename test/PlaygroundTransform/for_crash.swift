// RUN: rm -rf %t && mkdir -p %t
// RUN: cp %s %t/main.swift
// RUN: not %target-swift-frontend -typecheck -playground %t/main.swift
// RUN: not %target-swift-frontend -typecheck -playground -Xfrontend -pc-macro %t/main.swift

for x in y {
  x
}
