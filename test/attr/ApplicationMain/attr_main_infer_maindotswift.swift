// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t/main.swift %t/helper.swift

//--- main.swift

@main
struct Entry {
  static func main() {
    hi()
  }
}

//--- helper.swift

func hi() {}
