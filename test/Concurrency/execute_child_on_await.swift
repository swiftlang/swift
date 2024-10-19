// RUN: %target-run-simple-swift(-parse-as-library -target %target-swift-5.1-abi-triple -Xfrontend -concurrency-model=task-to-thread -g -Xlinker -object_path_lto -Xlinker /tmp/abc.o)

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: freestanding
// REQUIRES: concurrency_runtime

@_spi(_TaskToThreadModel) import _Concurrency
import StdlibUnittest
import Darwin

var a = 0;

func foo() async -> Int {
  a += 1
  print("a is now \(a)")
  return a
}

func bar() async -> Int {
  a += 2
  print("a is now \(a)")
  return a
}

func asyncFib(_ n: Int, _ expected_thread: pthread_t) async -> Int {
  expectEqual(pthread_self(), expected_thread);

  if n == 0 || n == 1 {
    return n
  }

  async let first = asyncFib(n-2, expected_thread)
  async let second = asyncFib(n-1, expected_thread)

  return (await second) + (await first)
}

func fib(_ n: Int) -> Int {
  var first = 0
  var second = 1
  for _ in 0..<n {
    let temp = first
    first = second
    second = temp + first
  }
  return first
}

@main struct Main {
  static func main() {

    let tests = TestSuite("Execute child task on await")
    tests.test("Basic async let only execute on await") {
      Task.runInline {
         async let a = foo()
         async let b = bar()

         expectEqual(await b, 2)
         expectEqual(await a, 3)
         // Re-querying a completed task should return original result and not
         // reexecute it
         expectEqual(await b, 2)
      }
    }

    tests.test("Nested async lets that only execute on await") {
      Task.runInline {
        let result = await asyncFib(10, pthread_self())
        expectEqual(result, fib(10))
      }
    }

    tests.test("Task group execute child task on await simple") {
      a = 0
      Task.runInline {
        await withTaskGroup(of: Int.self, body : { group in
          let currentThread = pthread_self()
          group.addTask {
            expectEqual(pthread_self(), currentThread);
            return await foo()
          }

          group.addTask {
            expectEqual(pthread_self(), currentThread);
            return await bar()
          }
        })
      }
    }

    tests.test("Task groups with nested structured concurrency") {
      Task.runInline {
        await withTaskGroup(of: Int.self, body : { group in
          let currentThread = pthread_self()
          group.addTask {
            return await asyncFib(10, currentThread);
          }

          group.addTask {
            return await asyncFib(5, currentThread);
          }
        })
      }
    }

    runAllTests()
  }
}
