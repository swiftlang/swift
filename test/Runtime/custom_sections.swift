// RUN: %target-run-simple-swift(-parse-as-library)
// REQUIRES: executable_test

// The query is back-deployed, so this also runs against an older OS stdlib,
// where the client-side fallback walks dyld's image list.

import StdlibUnittest

#if objectFormat(MachO)
let pluginsSection: StaticString = "__DATA_CONST,__myplugins"
let missingSection: StaticString = "__DATA_CONST,__nosuch"
@section("__DATA_CONST,__myplugins") @used var record0: (Int, Int) = (1, 2)
@section("__DATA_CONST,__myplugins") @used var record1: (Int, Int) = (3, 4)
#elseif objectFormat(COFF)
let pluginsSection: StaticString = ".myplugs$B"
let missingSection: StaticString = ".nosuch$B"
@section(".myplugs$B") @used var record0: (Int, Int) = (1, 2)
@section(".myplugs$B") @used var record1: (Int, Int) = (3, 4)
#else
let pluginsSection: StaticString = "myplugins"
let missingSection: StaticString = "nosuchsection"
@section("myplugins") @used var record0: (Int, Int) = (1, 2)
@section("myplugins") @used var record1: (Int, Int) = (3, 4)
#endif

@main
struct Main {
  static func main() {
    let suite = TestSuite("CustomSections")

    suite.test("customSectionContents(named:) returns this image's section") {
      let buffers = customSectionContents(named: pluginsSection)
      expectGE(buffers.count, 1)

      let total = buffers.reduce(0) { $0 + $1.count }
      expectGE(total, 2 * MemoryLayout<(Int, Int)>.stride)

      let records = buffers.flatMap { buffer in
        Array(buffer.bindMemory(to: (Int, Int).self))
      }
      expectTrue(records.contains { $0 == (1, 2) })
      expectTrue(records.contains { $0 == (3, 4) })
    }

    suite.test("customSectionContents(named:) is empty for an unknown section") {
      expectEqual(0, customSectionContents(named: missingSection).count)
    }

    suite.test("customSectionContents(named:) rejects a malformed name") {
      expectEqual(0, customSectionContents(named: "").count)
      expectEqual(0, customSectionContents(named: ",").count)
    }

    runAllTests()
  }
}
