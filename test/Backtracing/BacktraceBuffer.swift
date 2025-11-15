// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -cxx-interoperability-mode=default -Onone -o %t/BacktraceBuffer
// RUN: %target-codesign %t/BacktraceBuffer
// RUN: %target-run %t/BacktraceBuffer | %FileCheck %s

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx || OS=linux-gnu

#if os(macOS)
import Darwin
#else
import Glibc
#endif

@_spi(MemoryReaders)
@_spi(Contexts)
@_spi(BacktraceBuffer)
@_spi(Internal)
import Runtime

class SimpleFileSequence: Sequence, IteratorProtocol {
  public typealias Element = UInt8

  var fd: Int32
  var buffer: UnsafeMutableRawBufferPointer
  var readNdx, count: Int
  var _atEnd: Bool?

  init(path: String) {
    fd = open(path, O_RDONLY)
    buffer = .allocate(byteCount: 65536, alignment: 4096)
    readNdx = 0
    count = 0
    _atEnd = nil
  }
  deinit {
    buffer.deallocate()
    close(fd)
  }

  var atEnd: Bool {
    if readNdx < count {
      return false
    }

    if let _atEnd, _atEnd {
      return true
    }

    let done = read(fd, buffer.baseAddress, buffer.count)
    if done == 0 {
      _atEnd = true
    } else {
      count = done
      readNdx = 0
      _atEnd = false
    }

    return _atEnd!
  }

  public func next() -> UInt8? {
    // This has the side-effect of refilling the buffer
    if atEnd {
      return nil
    }

    let byte = buffer[readNdx]
    readNdx += 1

    return byte
  }
}

@main
struct BacktraceBufferTest {
  static var buffer: BacktraceBuffer<HostContext, UnsafeLocalMemoryReader>! = nil

  static func doFrames(_ count: Int, originalCount: Int) {
    if count <= 0 {
      let before = buffer.count
      HostContext.withCurrentContext { ctx in
        try! buffer.capture(from: ctx)
      }
      let used = buffer.count - before
      print("Captured \(originalCount) frames in \(used) bytes")
    } else {
      doFrames(count - 1, originalCount: originalCount)
    }
  }

  static func doFrames(_ count: Int) {
    doFrames(count, originalCount: count)
  }

  static func dumpCaptureFile() {
    let captured = SimpleFileSequence(path: "captured.bin")
    var count = 0

    while !captured.atEnd {
      print("Backtrace \(count):")
      count += 1

      let backtrace = Backtrace(architecture: HostContext.architecture,
                                compactBacktraceData: captured,
                                images: nil)

      print(backtrace)

      print("")
    }
  }

  static func main() {
    let fd = open("captured.bin", O_CREAT|O_TRUNC|O_RDWR, 0o644)
    defer { close(fd) }

    if fd < 0 {
      fatalError("Unable to create capture file: \(errno)")
    }

    buffer = BacktraceBuffer<HostContext, UnsafeLocalMemoryReader>(
      using: UnsafeLocalMemoryReader()
    ) { chunk in
      let done = write(fd, chunk.baseAddress, chunk.count)
      if done < 0 {
        print("Error \(errno)")
      } else {
        print("Wrote \(done) bytes out of \(chunk.count)")
      }
      return done
    }

    // CHECK: Backtrace 0:
    // CHECK: 0 0x{{[0-9a-f]*}}
    // CHECK: 1000 0x{{[0-9a-f]*}} [ra]
    doFrames(1000)

    // CHECK: Backtrace 1:
    // CHECK: 0 0x{{[0-9a-f]*}}
    // CHECK: 500 0x{{[0-9a-f]*}} [ra]
    doFrames(500)

    // CHECK: Backtrace 2:
    // CHECK: 0 0x{{[0-9a-f]*}}
    // CHECK: 250 0x{{[0-9a-f]*}} [ra]
    doFrames(250)

    // CHECK: Backtrace 3:
    // CHECK: 0 0x{{[0-9a-f]*}}
    // CHECK: 125 0x{{[0-9a-f]*}} [ra]
    doFrames(125)

    // CHECK: Backtrace 4:
    // CHECK: 0 0x{{[0-9a-f]*}}
    // CHECK: 63 0x{{[0-9a-f]*}} [ra]
    doFrames(63)

    // CHECK: Backtrace 5:
    // CHECK: 0 0x{{[0-9a-f]*}}
    // CHECK: 32 0x{{[0-9a-f]*}} [ra]
    doFrames(32)

    // CHECK: Backtrace 6:
    // CHECK: 0 0x{{[0-9a-f]*}}
    // CHECK: 16 0x{{[0-9a-f]*}} [ra]
    doFrames(16)

    // CHECK: Backtrace 7:
    // CHECK: 0 0x{{[0-9a-f]*}}
    // CHECK: 8 0x{{[0-9a-f]*}} [ra]
    doFrames(8)

    // CHECK: Backtrace 8:
    // CHECK: 0 0x{{[0-9a-f]*}}
    // CHECK: 4 0x{{[0-9a-f]*}} [ra]
    doFrames(4)

    // CHECK: Backtrace 9:
    // CHECK: 0 0x{{[0-9a-f]*}}
    // CHECK: 2 0x{{[0-9a-f]*}} [ra]
    doFrames(2)

    // CHECK: Backtrace 10:
    // CHECK: 0 0x{{[0-9a-f]*}}
    // CHECK: 1 0x{{[0-9a-f]*}} [ra]
    doFrames(1)

    // CHECK-NOT: Backtrace 11:

    try! buffer.flush()

    let size = lseek(fd, 0, SEEK_END)
    print("Used \(size) bytes")

    dumpCaptureFile()
  }
}
