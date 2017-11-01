// RUN: %target-build-swift %S/Inputs/library.swift %S/main.swift
// RUN: %target-build-swift -whole-module-optimization %S/Inputs/library.swift %S/main.swift

func meltCheese(_ burger: Burger) -> Int {
  return burger.cheeseSlices
}
