// RUN: %target-swift-frontend -typecheck -primary-file %s %S/Inputs/protocol-inheritance.swift

func kitty<Purrs>(cat: Meow<Purrs>) {
  cat.pet()
}
