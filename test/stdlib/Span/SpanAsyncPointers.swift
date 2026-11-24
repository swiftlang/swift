// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest
import Dispatch

/// Suspends the task and runs `body` on another thread before resuming it,
/// like a C library or the kernel that uses a buffer until it completes.
func onOtherThread<T: Sendable>(_ body: @escaping @Sendable () -> T) async -> T {
  await withCheckedContinuation { continuation in
    DispatchQueue.global().async {
      continuation.resume(returning: body())
    }
  }
}

struct Pointer: @unchecked Sendable {
  var raw: UnsafeMutableRawPointer
}

struct NonCopyable: ~Copyable {
  var value: Int
}

enum TestError: Error, Equatable {
  case failed
}

actor IsolationChecker {
  func readBytes(of array: [UInt8]) async -> Int {
    await array.span.withUnsafeBytes { bytes in
      await Task.yield()
      // The closure runs in the isolation of the caller.
      self.preconditionIsolated()
      return bytes.count
    }
  }
}

@main struct Main {
  static func main() async {
    let suite = TestSuite("SpanAsyncPointers")

    suite.test("Span.withUnsafeBufferPointer") {
      let array = [1, 2, 3, 4]
      let span = array.span
      let expected = span.withUnsafeBufferPointer { $0.baseAddress }
      let sum = await span.withUnsafeBufferPointer { buffer in
        expectEqual(buffer.baseAddress, expected)
        expectEqual(buffer.count, 4)
        let pointer = Pointer(raw: UnsafeMutableRawPointer(mutating: buffer.baseAddress!))
        return await onOtherThread {
          UnsafeBufferPointer(
            start: pointer.raw.assumingMemoryBound(to: Int.self), count: 4
          ).reduce(0, +)
        }
      }
      expectEqual(sum, 10)
    }

    suite.test("Span.withUnsafeBufferPointer of non-copyable elements") {
      let buffer = UnsafeMutableBufferPointer<NonCopyable>.allocate(capacity: 2)
      defer { buffer.deallocate() }
      buffer.initializeElement(at: 0, to: NonCopyable(value: 1))
      buffer.initializeElement(at: 1, to: NonCopyable(value: 2))
      defer { _ = buffer.deinitialize() }
      let span = Span(_unsafeElements: buffer)
      let sum = await span.withUnsafeBufferPointer { elements in
        await Task.yield()
        return elements[0].value + elements[1].value
      }
      expectEqual(sum, 3)
    }

    suite.test("Span.withUnsafeBytes") {
      let array: [UInt8] = [1, 2, 3]
      let span = array.span
      let count = await span.withUnsafeBytes { bytes in
        await Task.yield()
        return bytes.count
      }
      expectEqual(count, 3)
    }

    suite.test("Empty Span") {
      let array: [Int] = []
      let span = array.span
      let count = await span.withUnsafeBufferPointer { buffer in
        await Task.yield()
        return buffer.count
      }
      expectEqual(count, 0)
    }

    suite.test("MutableSpan.withUnsafeBufferPointer") {
      var array = [1, 2, 3]
      let span = array.mutableSpan
      let last = await span.withUnsafeBufferPointer { buffer in
        await Task.yield()
        return buffer.last
      }
      expectEqual(last, 3)
    }

    suite.test("MutableSpan.withUnsafeMutableBufferPointer") {
      var array = [0, 0, 0]
      var span = array.mutableSpan
      await span.withUnsafeMutableBufferPointer { buffer in
        let pointer = Pointer(raw: UnsafeMutableRawPointer(buffer.baseAddress!))
        await onOtherThread {
          let elements = pointer.raw.assumingMemoryBound(to: Int.self)
          for i in 0..<3 { elements[i] = i + 1 }
        }
      }
      _ = consume span
      expectEqual(array, [1, 2, 3])
    }

    suite.test("MutableSpan.withUnsafeBytes") {
      var array: [UInt16] = [1, 2]
      let span = array.mutableSpan
      let count = await span.withUnsafeBytes { bytes in
        await Task.yield()
        return bytes.count
      }
      expectEqual(count, 4)
    }

    suite.test("MutableSpan.withUnsafeMutableBytes") {
      var array: [UInt8] = [0, 0]
      var span = array.mutableSpan
      await span.withUnsafeMutableBytes { bytes in
        let pointer = Pointer(raw: bytes.baseAddress!)
        await onOtherThread {
          pointer.raw.storeBytes(of: 7, toByteOffset: 1, as: UInt8.self)
        }
      }
      _ = consume span
      expectEqual(array, [0, 7])
    }

    suite.test("MutableSpan typed throws") {
      var array = [1]
      var span = array.mutableSpan
      do {
        try await span.withUnsafeMutableBufferPointer { _ async throws(TestError) in
          await Task.yield()
          throw TestError.failed
        }
        expectUnreachable()
      } catch {
        expectEqual(error as? TestError, .failed)
      }
    }

    suite.test("OutputSpan.withUnsafeMutableBufferPointer") {
      let buffer = UnsafeMutableBufferPointer<Int>.allocate(capacity: 4)
      defer { buffer.deallocate() }
      var output = OutputSpan(buffer: buffer, initializedCount: 0)
      output.append(1)
      let written = await output.withUnsafeMutableBufferPointer { elements, initializedCount in
        expectEqual(initializedCount, 1)
        let pointer = Pointer(raw: UnsafeMutableRawPointer(elements.baseAddress!))
        let start = initializedCount
        await onOtherThread {
          let elements = pointer.raw.assumingMemoryBound(to: Int.self)
          (elements + start).initialize(repeating: 9, count: 2)
        }
        initializedCount += 2
        return 2
      }
      expectEqual(written, 2)
      expectEqual(output.count, 3)
      let count = output.finalize(for: buffer)
      expectEqual(count, 3)
      expectEqual(Array(buffer.prefix(count)), [1, 9, 9])
      _ = buffer.prefix(count).deinitialize()
    }

    suite.test("OutputSpan count is updated when the closure throws") {
      let buffer = UnsafeMutableBufferPointer<Int>.allocate(capacity: 2)
      defer { buffer.deallocate() }
      var output = OutputSpan(buffer: buffer, initializedCount: 0)
      do {
        try await output.withUnsafeMutableBufferPointer { elements, initializedCount async throws(TestError) in
          elements.initializeElement(at: 0, to: 5)
          initializedCount = 1
          await Task.yield()
          throw TestError.failed
        }
        expectUnreachable()
      } catch {
        expectEqual(error as? TestError, .failed)
      }
      expectEqual(output.count, 1)
      let count = output.finalize(for: buffer)
      expectEqual(count, 1)
      _ = buffer.prefix(count).deinitialize()
    }

    suite.test("RawSpan.withUnsafeBytes") {
      let array: [UInt8] = [1, 2, 3]
      let span = array.span.bytes
      let sum = await span.withUnsafeBytes { bytes in
        let pointer = Pointer(raw: UnsafeMutableRawPointer(mutating: bytes.baseAddress!))
        return await onOtherThread {
          UnsafeRawBufferPointer(start: pointer.raw, count: 3).reduce(0) { $0 + Int($1) }
        }
      }
      expectEqual(sum, 6)
    }

    suite.test("MutableRawSpan.withUnsafeBytes") {
      var array: [UInt8] = [4, 5]
      var mutableSpan = array.mutableSpan
      let span = mutableSpan.mutableBytes
      let first = await span.withUnsafeBytes { bytes in
        await Task.yield()
        return bytes.first
      }
      expectEqual(first, 4)
    }

    suite.test("MutableRawSpan.withUnsafeMutableBytes") {
      var array: [UInt8] = [0, 0, 0]
      var mutableSpan = array.mutableSpan
      var span = mutableSpan.mutableBytes
      await span.withUnsafeMutableBytes { bytes in
        let pointer = Pointer(raw: bytes.baseAddress!)
        await onOtherThread {
          pointer.raw.storeBytes(of: 3, toByteOffset: 2, as: UInt8.self)
        }
      }
      _ = consume span
      _ = consume mutableSpan
      expectEqual(array, [0, 0, 3])
    }

    suite.test("OutputRawSpan.withUnsafeMutableBytes") {
      let buffer = UnsafeMutableRawBufferPointer.allocate(byteCount: 4, alignment: 1)
      defer { buffer.deallocate() }
      var output = OutputRawSpan(buffer: buffer, initializedCount: 0)
      output.append(UInt8(1))
      let written = await output.withUnsafeMutableBytes { bytes, initializedCount in
        let pointer = Pointer(raw: bytes.baseAddress!)
        let start = initializedCount
        await onOtherThread {
          pointer.raw.storeBytes(of: 2, toByteOffset: start, as: UInt8.self)
          pointer.raw.storeBytes(of: 3, toByteOffset: start + 1, as: UInt8.self)
        }
        initializedCount += 2
        return 2
      }
      expectEqual(written, 2)
      expectEqual(output.byteCount, 3)
      let count = output.finalize(for: buffer)
      expectEqual(Array(buffer.prefix(count)), [1, 2, 3])
    }

    suite.test("Non-copyable return value") {
      let array = [42]
      let value = await array.span.withUnsafeBufferPointer { buffer in
        await Task.yield()
        return NonCopyable(value: buffer[0])
      }
      expectEqual(value.value, 42)
    }

    suite.test("The closure runs in the isolation of the caller") {
      let count = await IsolationChecker().readBytes(of: [1, 2, 3])
      expectEqual(count, 3)
    }

    await runAllTestsAsync()
  }
}
