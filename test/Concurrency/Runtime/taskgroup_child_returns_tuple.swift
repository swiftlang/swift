// RUN: %target-run-simple-swift(-parse-as-library -Onone) | %FileCheck %s
// RUN: %target-run-simple-swift(-parse-as-library -O) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// swiftlang/swift#92698: at -O, a task-group child returning a tuple lost the
// captured key on the success path. The key was stored into the indirect
// result buffer in the entry block, but the success block stored the rest of
// the tuple without re-storing the key. Because the async return region ended
// in `unreachable`, DSE treated those stores as unobservable and deleted the
// entry store, so the parent read back a wrong key for successful children.

enum Key: Int {
  case a
  case b
  case c
}

struct FetchError: Error {}

@inline(never)
func fetch(_ key: Key) async throws -> Int {
  await Task.yield()
  if key == .b {
    throw FetchError()
  }
  return key.rawValue * 10
}

@inline(never)
func grouped() async -> [(Key, Result<Int, Error>)] {
  await withTaskGroup(of: (Key, Result<Int, Error>).self) { group in
    for key in [Key.a, Key.b, Key.c] {
      group.addTask {
        do {
          return (key, .success(try await fetch(key)))
        } catch {
          return (key, .failure(error))
        }
      }
    }

    var results: [(Key, Result<Int, Error>)] = []
    for await result in group {
      results.append(result)
    }
    return results.sorted { $0.0.rawValue < $1.0.rawValue }
  }
}

@main
struct Main {
  static func main() async {
    let results = await grouped()
    print(results.map { $0.0 }) // CHECK: [a, b, c]
    print(results.map { try? $0.1.get() }) // CHECK: [Optional(0), nil, Optional(20)]
    print("done") // CHECK: done
  }
}
