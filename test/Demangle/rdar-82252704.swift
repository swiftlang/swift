// rdar://82252704 - [SR-15070]: Declaring a class inside a async throws
//                   function crashes compiler

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c %s -o %t/test.o

@available(SwiftStdlib 5.1, *)
func MyFunction() async throws {
    class MyClass {}
}
