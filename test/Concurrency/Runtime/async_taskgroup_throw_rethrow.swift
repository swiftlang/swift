// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) | %FileCheck %s --dump-input always
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64

import Dispatch

struct Boom: Error {}
struct IgnoredBoom: Error {}

func echo(_ i: Int) async -> Int { i }
func boom() async throws -> Int { throw Boom() }

func test_taskGroup_throws_rethrows() async {
  do {
    let got = await try Task.withGroup(resultType: Int.self) { (group) async throws -> Int in
      await group.add { await echo(1) }
      await group.add { await echo(2) }
      await group.add { await try boom() }

      do {
        while let r = await try group.next() {
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
runAsyncAndBlock(test_taskGroup_throws_rethrows)
