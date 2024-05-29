// RUN: %target-run-simple-swift( -plugin-path %swift-plugin-dir -Xfrontend -disable-availability-checking -parse-as-library %import-libdispatch) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

@available(SwiftStdlib 5.1, *)
enum TL {
  @TaskLocal
  static var one: Int = 1
  @TaskLocal
  static var two: Int = 2
  @TaskLocal
  static var three: Int = 3
}

// ==== ------------------------------------------------------------------------

func test() async {
  await TL.$one.withValue(11) {
    await TL.$one.withValue(1111) {
      await withTaskGroup(of: Void.self) { group in
        TL.$two.withValue(2222) {
          group.addTask { // will have to copy the `2222`
            print("Survived, one: \(TL.one) @ \(#fileID):\(#line)") // CHECK: Survived, one: 1111
            print("Survived, two: \(TL.two) @ \(#fileID):\(#line)") // CHECK: Survived, two: 2222
          }
        }

        await group.next()
        print("--")

        TL.$two.withValue(2) {
          TL.$two.withValue(22) {
            TL.$two.withValue(2222) {
              group.addTask { // will have to copy the `2222`
                print("Survived, one: \(TL.one) @ \(#fileID):\(#line)") // CHECK: Survived, one: 1111
                print("Survived, two: \(TL.two) @ \(#fileID):\(#line)") // CHECK: Survived, two: 2222
              }
            }
          }
        }
        await group.next()

      }

      print("Survived, done") // CHECK: Survived, done
    }
  }
}

@main struct Main {
  static func main() async {
    await test()
  }
}
