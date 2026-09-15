// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t/main.swift %t/helper.swift

// The diagnostic doesn't depend on the order the files are given in, even though
// that determines whether the script file or the '@main' attribute claims the
// module's entry point first.
// RUN: %target-swift-frontend -typecheck -verify %t/helper.swift %t/main.swift

// Nor does it depend on which file is primary.
// RUN: %target-swift-frontend -typecheck -verify -primary-file %t/helper.swift %t/main.swift

// @main does not suppress top level code parsing if it doesn't appear in the "main" file

//--- main.swift
func hi() {} // expected-note {{top-level code defined in this source file}}
// expected-note@-1 {{pass '-parse-as-library' to compiler invocation if this is intentional}}

//--- helper.swift

@main // expected-error {{'main' attribute cannot be used in a module that contains top-level code}}
struct Entry {
  static func main() {
    hi()
  }
}
