// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

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
        // CHECK: error caught and rethrown in group: Boom()
        print("error caught and rethrown in group: \(error)")
        throw error
      }

      fatalError("should have thrown")
    }

      print("got: \(got)")
      fatalError("Expected error to be thrown, but got: \(got)")
  } catch {
    // CHECK: rethrown: Boom()
    print("rethrown: \(error)")
  }
}


@main struct Main {
  static func main() async {
    await test_taskGroup_throws_rethrows()
  }
}
