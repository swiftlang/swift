// This file is a part of the multi-file test driven by 'main.swift'.

// NB: No "-verify"--this file should parse successfully on its own.
// RUN: %target-swift-frontend -typecheck -parse-as-library %s

@main // expected-error{{'main' attribute cannot be used in a module that contains top-level code}}
class MyMain {
  static func main() {
  }
}

func hi() {}
