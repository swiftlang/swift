// RUN: %target-run-simple-swift( -plugin-path %swift-plugin-dir -Xfrontend -strict-concurrency=complete -Xfrontend -disable-availability-checking -parse-as-library %import-libdispatch) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

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
  // no outer task locals
  await withTaskGroup(of: Void.self) { group in
    TL.$two.withValue(2222) {
      // should not have any effect on reads below
    }
    await TL.$two.withValue(22) {
      group.addTask { // will have to copy the `22`
        print("Survived, one: \(TL.one) @ \(#fileID):\(#line)") // CHECK: Survived, one: 1
        print("Survived, two: \(TL.two) @ \(#fileID):\(#line)") // CHECK: Survived, two: 22
      }
    }
  }

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
        print("--")

        TL.$two.withValue(2) {
          TL.$two.withValue(22) {
            TL.$three.withValue(33) {
              TL.$two.withValue(2222) {
                group.addTask { // will have to copy the `2222`
                  print("Survived, one: \(TL.one) @ \(#fileID):\(#line)") // CHECK: Survived, one: 1111
                  print("Survived, two: \(TL.two) @ \(#fileID):\(#line)") // CHECK: Survived, two: 2222

                  TL.$three.withValue(3333) {
                    print("Survived, one: \(TL.one) @ \(#fileID):\(#line)") // CHECK: Survived, one: 1111
                    print("Survived, two: \(TL.two) @ \(#fileID):\(#line)") // CHECK: Survived, two: 2222
                    print("Survived, three: \(TL.three) @ \(#fileID):\(#line)") // CHECK: Survived, three: 3333
                  }
                }
              }
            }
          }
        }
        await group.next()
        print("--")
      }
    }
  }

  await TL.$one.withValue(11) {
    await Task {
      async let x = await withTaskGroup(of: Void.self) { group in
        TL.$two.withValue(22) {
          group.addTask { // will have to copy the `2222`
            print("Survived, one: \(TL.one) @ \(#fileID):\(#line)") // CHECK: Survived, one: 11
            print("Survived, two: \(TL.two) @ \(#fileID):\(#line)") // CHECK: Survived, two: 22
          }
        }
      }
    }
      .value
    print("--")
  }

  await TL.$one.withValue(11) {
    await withTaskGroup(of: Void.self) { group in
      await TL.$three.withValue(33) {
        await withTaskGroup(of: Void.self) { group in
          TL.$two.withValue(2222) {
            group.addTask { // will have to copy the `2222`
              print("Survived, one: \(TL.one) @ \(#fileID):\(#line)") // CHECK: Survived, one: 11
              print("Survived, two: \(TL.two) @ \(#fileID):\(#line)") // CHECK: Survived, two: 2222
              print("Survived, three: \(TL.three) @ \(#fileID):\(#line)") // CHECK: Survived, three: 33
            }
          }
        }
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
