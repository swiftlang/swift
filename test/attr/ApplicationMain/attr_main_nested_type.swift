// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify %t/nested_in_struct.swift
// RUN: %target-swift-frontend -typecheck -verify %t/deeply_nested.swift
// RUN: %target-swift-frontend -typecheck -verify %t/in_extension.swift
// RUN: %target-swift-frontend -typecheck -verify %t/in_function_body.swift

//--- nested_in_struct.swift

struct Outer {
  @main
  struct Entry {
    static func main() {
      print(greeting)
    }
  }
}

let greeting = "hello"

//--- deeply_nested.swift

func takesClosure(_ f: () -> String) -> String { f() }

enum Outer {
  enum Middle {
    @main
    struct Entry {
      static func main() {
        print(greeting)
      }
    }
  }
}

let greeting = takesClosure { "hello" }

//--- in_extension.swift

struct Outer {}

extension Outer {
  @main
  final class Entry {
    static func main() {
      print(greeting)
    }
  }
}

let greeting = "hello"

//--- in_function_body.swift

func takesClosure(_ f: () -> String) -> String { f() }

func nestsTheEntryPoint() {
  @main
  struct Entry {
    static func main() {
      print(greeting)
    }
  }
}

let greeting = takesClosure { "hello" }
