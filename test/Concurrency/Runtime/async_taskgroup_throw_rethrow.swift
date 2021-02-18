// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library) | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: concurrency

// REQUIRES: rdar73154198

import Dispatch

struct Boom: Error {}
struct IgnoredBoom: Error {}

func echo(_ i: Int) async -> Int { i }
func boom() async throws -> Int { throw Boom() }

func test_taskGroup_throws_rethrows() async {
  do {
    let got = try await Task.withGroup(resultType: Int.self) { (group) async throws -> Int in
      await group.add { await echo(1) }
      await group.add { await echo(2) }
      await group.add { try await boom() }

      do {
        while let r = try await group.next() {
          print("next: \(r)")
        }
      } catch {
        print("error caught and rethrown in group: \(error)")
        throw error
      }

      fatalError("should have thrown")
    }

      print("got: \(got)")
      fatalError("Expected error to be thrown, but got: \(got)")
  } catch {
    print("rethrown: \(error)")
  }
}


// CHECK: error caught and rethrown in group: Boom()
// CHECK: rethrown: Boom()
@main struct Main {
  static func main() async {
    await test_taskGroup_throws_rethrows()
  }
}
