// The `swift_createExecutors()` call that installs `DefaultExecutorFactory` is
// only emitted with availability checking disabled, because the
// `CustomGlobalExecutors` feature availability is still `FUTURE`.
// RUN: %target-run-simple-swift(-parse-as-library -Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: OS=linux-gnu

// The toolchain has the experimental io_uring backend of the platform executors
// (`--enable-platform-executors-io-uring`).
// REQUIRES: platform_executors_io_uring
// The kernel lets this machine create an io_uring. Containers often don't.
// REQUIRES: io_uring

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

@_spi(ExperimentalScheduling) @_spi(ConcurrencyExecutors) @_spi(ExperimentalCustomExecutors) import PlatformExecutors
@_spi(ExperimentalCustomExecutors) import _Concurrency

import Glibc

/// Reads the file at `path` with POSIX calls, without the platform executors.
func readFile(_ path: String) -> String {
  let fd = open(path, O_RDONLY)
  guard fd >= 0 else { return "<open failed>" }
  defer { close(fd) }
  var buffer = [UInt8](repeating: 0, count: 64)
  let count = buffer.withUnsafeMutableBytes { read(fd, $0.baseAddress, $0.count) }
  guard count > 0 else { return "<read failed>" }
  return String(decoding: buffer[..<count], as: UTF8.self)
}

@main struct Main {
  static func main() async throws {
    // CHECK: backend: ioUring
    print("backend: \(PThreadIOBackend.ioUring)")

    // CHECK: available: true
    do {
      try PThreadIOBackend.ioUring.checkAvailability()
      print("available: true")
    } catch {
      print("available: false, \(error)")
    }

    let path = "/tmp/platform-executors-io-uring-\(getpid()).txt"
    defer { unlink(path) }

    // Select io_uring explicitly: `platformDefault` would fall back to epoll
    // when the kernel doesn't allow io_uring, and the test would silently not
    // test it. The io_uring backend performs the file operations on the ring.
    try await PThreadTaskExecutor.withExecutor(
      name: "pe-uring", poolSize: 2, ioBackend: .ioUring
    ) { executor in
      try await withTaskExecutorPreference(executor) {
        let file = try await File.Writable.open(
          path: path,
          options: .newFile(replaceExisting: true)
        )
        try await file.write(from: Array("hello io_uring".utf8).span.bytes, at: 0)
        try await file.close()
      }
    }

    // CHECK: contents: hello io_uring
    print("contents: \(readFile(path))")

    // CHECK: done
    print("done")
  }
}
