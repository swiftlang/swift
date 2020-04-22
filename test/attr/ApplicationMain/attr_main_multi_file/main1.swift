// This file is a part of the multi-file test driven by 'main2.swift'.

// NB: No "-verify"--this file should parse successfully on its own.
// RUN: %target-swift-frontend -typecheck -parse-as-library %s

@main // expected-error{{'main' attribute can only apply to one type in a module}}
class MyMain {
  static func main() {
  }
}

func hi() {}
