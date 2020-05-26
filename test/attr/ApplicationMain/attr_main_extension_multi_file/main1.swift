// This file is a part of the multi-file test driven by 'main2.swift'.

// RUN: %target-swift-frontend -parse %s

@main
extension Main {
  static func main() {
    print("hello world")
  }
}
