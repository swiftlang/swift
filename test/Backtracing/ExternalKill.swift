// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s Inputs/do_fork.c -parse-as-library -Onone -g -o %t/ExternalKill
// RUN: %target-codesign %t/ExternalKill

// RUN: ! env SWIFT_BACKTRACE=enable=yes,cache=no %target-run %t/ExternalKill

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: asan
// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=linux-gnu

// This test won't actually work on Darwin right now, because the waitpid()
// will return after the backtracer is done, and the process will exit
// normally.  That happens because we can't detect that we've been killed
// by an external process.

#if os(macOS)
import Darwin
#elseif os(Linux)
  #if canImport(Glibc)
  import Glibc
  #elseif canImport(Musl)
  import Musl
  #endif
#endif

// Work around fork being marked unavailable
@_silgen_name("do_fork")
func do_fork() -> Int32

@main
struct ExternalKill {
  static func main() {

    let parent = getpid()

    let child = do_fork()
    if child == -1 {
      perror("unable to fork")
      exit(1)
    }
    if child == 0 {
      kill(parent, SIGSEGV)
      _exit(0)
    }

    var status: CInt = 0
    _ = waitpid(child, &status, 0)

    exit(0)
  }
}