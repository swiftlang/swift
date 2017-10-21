// RUN: %target-build-swift %S/library.swift %S/main.swift
// RUN: %target-build-swift -whole-module-optimization %S/library.swift %S/main.swift

// REQUIRES: executable_test

func meltCheese(_ burger: Burger) -> Int {
  return burger.cheeseSlices
}
