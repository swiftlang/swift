// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature Extern -enable-experimental-feature Embedded -wmo %s -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

// Bind to libc's `vprintf` directly. Use @_extern(c) rather than
// @_silgen_name so the call uses the C calling convention: on aarch64
// Linux, CVaListPointer is a 32-byte struct that AAPCS64 requires to be
// passed by hidden pointer, and Swift's default convention doesn't match.
@_extern(c)
func vprintf(_ fmt: UnsafePointer<CChar>, _ va: CVaListPointer) -> CInt

// A Swift wrapper that takes CVarArg values and forwards them into vprintf.
func swiftPrintf(_ fmt: StaticString, _ args: CVarArg...) {
  fmt.withUTF8Buffer { buf in
    buf.baseAddress!.withMemoryRebound(to: CChar.self, capacity: buf.count) { cfmt in
      _ = withVaList(args) { va in vprintf(cfmt, va) }
    }
  }
}

@main
struct Main {
  static func main() {
    // 1. Forwarded through a Swift CVarArg... wrapper.
    swiftPrintf("[wrapper] %d + %d = %d\n", 40 as Int32, 2 as Int32, 42 as Int32)
    // CHECK: [wrapper] 40 + 2 = 42

    // 2. Direct withVaList on a [CVarArg] literal.
    _ = withVaList([1.5 as Double, 2.5 as Double]) { va in
      "[direct] %g and %g\n".withCString { fmt in
        vprintf(fmt, va)
      }
    }
    // CHECK: [direct] 1.5 and 2.5

    // 3. Mixed types, including a string pointer.
    "hello".withCString { s in
      _ = withVaList([Int32(3), OpaquePointer(s)]) { va in
        "[mixed] count=%d str=%s\n".withCString { fmt in
          vprintf(fmt, va)
        }
      }
    }
    // CHECK: [mixed] count=3 str=hello

    print("done")
    // CHECK: done
  }
}
