// Check that when emitting diagnostics in SourceKit, we don't report false positives in PerformanceDiagnostics (because WMO is disabled).

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %sourcekitd-test -req=diags %t/file1.swift -- %t/file1.swift %t/file2.swift -enable-experimental-feature Embedded -target %target-cpu-apple-macos14 | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: embedded_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

//--- file1.swift

func foo() {
    bar(Int.self)
}

func testNonCopyable() {
  let nc = NonCopyable()
  nc.doSomething()
}

@main
struct Main {
    var someClass = SomeClass()

    static func main() {
        foo()
    }
}

//--- file2.swift

final class SomeClass {}

func bar<T>(_ T: T.Type) {}

struct NonCopyable : ~Copyable {
  func doSomething() {}
  deinit {}
}

// CHECK:      {
// CHECK-NEXT:   key.diagnostics: [
// CHECK-NEXT:   ]
// CHECK-NEXT: }
