// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/this-is-my-executable-path.out
// RUN: %target-codesign %t/this-is-my-executable-path.out
// RUN: %target-run %t/this-is-my-executable-path.out
// REQUIRES: executable_test
// UNSUPPORTED: OS=macosx, OS=ios, OS=tvos, OS=watchos, OS=xros
// UNSUPPORTED: OS=wasip1, OS=emscripten

import StdlibUnittest

#if canImport(Darwin)
  import Darwin
#elseif canImport(Glibc)
  import Glibc
#elseif os(Windows)
  import WinSDK
#endif

var CommandLineExecutablePath = TestSuite("CommandLineExecutablePath")

extension CommandLine {
#if os(Windows)
  internal static var executablePathCString: ContiguousArray<CWideChar> {
    @_silgen_name("_swift_stdlib_executablePathCString") get
  }
#else
  internal static var executablePathCString: ContiguousArray<CChar> {
    @_silgen_name("_swift_stdlib_executablePathCString") get
  }
#endif
}

CommandLineExecutablePath.test("executablePathCapturedAndCorrect") {
#if os(Windows)
  CommandLine.executablePathCString.withUnsafeBufferPointer { path in
    #"\this-is-my-executable-path.out"#.withCString(encodedAs: UTF16.self) { subpath in
      expectNotEqual(nil, wcsstr(path.baseAddress!, subpath))
    }
  }
#else
  CommandLine.executablePathCString.withUnsafeBufferPointer { path in
    expectNotEqual(nil, strstr(path.baseAddress!, "/this-is-my-executable-path.out"))
  }
#endif
}

runAllTests()

