// RUN: %target-build-swift %S/Inputs/library.swift %S/main.swift
// RUN: %target-build-swift -whole-module-optimization %S/Inputs/library.swift %S/main.swift
// RUN: %target-build-swift -enable-library-evolution %S/Inputs/library.swift %S/main.swift
// RUN: %target-build-swift -enable-library-evolution -whole-module-optimization %S/Inputs/library.swift %S/main.swift

func meltCheese(_ burger: Burger) -> Int {
  return burger.cheeseSlices
}

@inlinable
func bestBurrito(_ burrito: Burrito) -> Int {
  return burrito.cilantro
}
