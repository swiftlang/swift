// RUN: %target-run-simple-swift(-parse-as-library -target %target-swift-5.1-abi-triple -Xfrontend -concurrency-model=task-to-thread -g -Xlinker -object_path_lto -Xlinker /tmp/abc.o)
// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// REQUIRES: freestanding
// UNSUPPORTED: threading_none

@_spi(_TaskToThreadModel) import _Concurrency
import StdlibUnittest
import Darwin

var globalContinuation : CheckedContinuation<Int, Never>? = nil

func waitOnContinuation(_unused : UnsafeMutableRawPointer) -> UnsafeMutableRawPointer? {
    Task.runInline {
      let result = await withCheckedContinuation { continuation in
          globalContinuation = continuation
      }
      print("Continuation successfully resumed")
      expectEqual(result, 10)
    }
    return nil
}

func resumeContinuation(_unused : UnsafeMutableRawPointer) -> UnsafeMutableRawPointer? {
    Task.runInline {
      while (globalContinuation == nil) {}
      globalContinuation!.resume(returning: 10)
    }
    return nil
}

@main struct Main {
  static func main() {

    let tests = TestSuite("Continuations in task-to-thread")
    tests.test("Basic continuations - no blocking") {
      Task.runInline {
        await withCheckedContinuation { continuation in
          continuation.resume()
        }
      }
    }

    tests.test("Continuations - with blocking") {
      var thread1 : pthread_t? = nil
      guard pthread_create(&thread1, nil, waitOnContinuation, nil) == 0 else {
        fatalError("pthread_create failed")
      }

      var thread2 : pthread_t? = nil
      guard pthread_create(&thread2, nil, resumeContinuation, nil) == 0 else {
        fatalError("pthread_create failed")
      }

      guard pthread_join(thread1!, nil) == 0 else {
        fatalError("pthread_join failed")
      }
      guard pthread_join(thread2!, nil) == 0 else {
        fatalError("pthread_join failed")
      }
    }

    runAllTests()
  }
}
