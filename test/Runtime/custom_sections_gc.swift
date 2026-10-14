// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -parse-as-library -use-ld=lld -Xlinker --gc-sections %s -o %t/gc
// RUN: %target-codesign %t/gc
// RUN: %target-run %t/gc | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: OS=linux-gnu

// A record without '@used' that nothing references is removed by the linker
// under --gc-sections ('public' keeps the optimizer from removing it first, and
// -O keeps -Onone's debugger preservation from marking it used). With lld's
// default '-z start-stop-gc' the index's reference does not retain the section,
// so without the compiler's anchor __start_gcplugins would be undefined and the
// link would fail. The link must succeed and the kept record must be found.

@section("gcplugins") public var dropped: Int = 1
@section("gcplugins") @used var kept: Int = 2

@main
struct Main {
  static func main() {
    let buffers = customSectionContents(named: "gcplugins")
    let total = buffers.reduce(0) { $0 + $1.count }
    // CHECK: linked and queried
    print("linked and queried")
    // CHECK: kept found: true
    let values = buffers.flatMap { Array($0.bindMemory(to: Int.self)) }
    print("kept found: \(values.contains(2))")
    print("bytes: \(total)")
  }
}
