// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch
import Foundation // TODO: remove

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
enum TL {
  @TaskLocal
  static var number: Int  = 0
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@discardableResult
func printTaskLocal<V, Key>(
    _ key: Key,
    _ expected: V? = nil,
    file: String = #file, line: UInt = #line
) -> V? where Key: TaskLocal<V> {
  let value = key
  print("\(value) at \(file):\(line)")
  if let expected = expected {
    assert("\(expected)" == "\(value)",
        "Expected [\(expected)] but found: \(value), at \(file):\(line)")
  }
  return expected
}

func pprint(_ s: String, file: String = #file, line: UInt = #line) {
  print("\(s)    at :\(line)")
  fputs("[\(file):\(line)] \(s)\n", stderr)
}

// ==== ------------------------------------------------------------------------

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func bindAroundGroupSpawn() async {
  await TL.$number.withValue(1111) {
    await withTaskGroup(of: Int.self) { group in
      pprint("inside group-1: \(TL.$number)")

      pprint("before-withValue: going to set 2222")
      await TL.$number.withValue(2222) {
        pprint("inside-withValue: \(TL.$number)")

        group.spawn {
          pprint("inside group-child: \(TL.$number)")
          await Task.sleep(10_000)
          pprint("inside group-child after-sleep: \(TL.$number)")
          printTaskLocal(TL.$number)
          return TL.number
        }

        pprint("exit inside-withValue: \(TL.$number)")
      } // end: number=2222
      pprint("done inside-withValue: \(TL.$number)")

      // CHECK: NEIN
      guard let v = await group.next() else {
        print("No value!")
        return
      }
      _ = v

    } // end: withTaskGroup
  } // end: number=1111
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@main struct Main {
  static func main() async {
    await bindAroundGroupSpawn()
  }
}
