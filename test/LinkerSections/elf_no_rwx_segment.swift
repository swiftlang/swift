// Check that ordinary Swift code does not cause GNU ld to produce an RWX
// LOAD segment on AArch64 Linux (rdar://186556339,
// https://github.com/swiftlang/swift/issues/91943).
//
// This links with the default linker and its default options; no particular
// linker is assumed to be installed. With GNU ld on AArch64, which defaults to
// -z noseparate-code, .rodata is placed in the same LOAD segment as .text, so
// any writable .rodata section emitted by the compiler makes that segment RWX.

// REQUIRES: OS=linux-gnu
// REQUIRES: CPU=aarch64
// REQUIRES: executable_test

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O %s -o %t/main.o -c -parse-as-library -module-name main
// RUN: %llvm-readelf -SW %t/main.o | %FileCheck %s --check-prefix=SECTIONS

// RUN: %target-build-swift -O %s -o %t/a.out -parse-as-library -module-name main 2>&1 | %FileCheck %s --check-prefix=LINK --allow-empty
// RUN: %llvm-readelf -lW %t/a.out | %FileCheck %s --check-prefix=SEGMENTS
// RUN: %llvm-readelf -SW %t/a.out | %FileCheck %s --check-prefix=SECTIONS
// RUN: %target-run %t/a.out | %FileCheck %s --check-prefix=OUTPUT

// LINK-NOT: RWX

// SECTIONS-NOT: {{\.rodata +PROGBITS +[0-9a-f]+ [0-9a-f]+ [0-9a-f]+ [0-9a-f]+ +[A-Z]*W}}

// SEGMENTS: LOAD
// SEGMENTS-NOT: {{LOAD .* RWE}}

// Synthesized CodingKeys: CodingKey and Hashable (refining Equatable).
struct Record: Codable, Hashable {
  let name: String
  let count: Int
}

// Comparable refines Equatable.
struct Version: Comparable {
  let major: Int
  static func < (lhs: Version, rhs: Version) -> Bool { lhs.major < rhs.major }
}

struct Box<T> {
  var value: T
}

@inline(never)
func readThroughKeyPath<Root, Value>(_ root: Root, _ kp: KeyPath<Root, Value>) -> Value {
  root[keyPath: kp]
}

@inline(never)
func genericKeyPath<T>(_: T.Type) -> KeyPath<Box<T>, T> {
  \Box<T>.value
}

@main
struct Main {
  static func main() {
    let r = Record(name: "hello", count: 1)
    // OUTPUT: hello 1 true
    print(r.name, readThroughKeyPath(r, \Record.count), r == r)
    // OUTPUT: true
    print(Version(major: 1) < Version(major: 2))
    // OUTPUT: 42
    print(readThroughKeyPath(Box(value: 42), genericKeyPath(Int.self)))
  }
}
