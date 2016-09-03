// RUN: rm -rf %t && mkdir -p %t
// RUN: cp %s %t/main.swift
// RUN: not %target-swift-frontend -parse -playground %t/main.swift

for x in y {
  x
}
