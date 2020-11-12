//// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
//// REQUIRES: concurrency
//
//import Foundation
//
//// ==== ------------------------------------------------------------------------
//// ==== API on Task style
//
//extension TaskLocalValues {
//  public struct Example: TaskLocalKey {
//    public static let defaultValue: String? = nil
//  }
//  public var example: Example { .init() }
//}
//
//extension TaskLocalValues {
//  public struct RequestIDString: TaskLocalKey {
//    public static var defaultValue: String? = nil
//  }
//  public var requestIDString: RequestIDString { .init() }
//}
//
//func async() async {
//  let thing = await Task.local(\.example)
//  _ = thing
//}
//
//struct APIOnTaskStyle {
//  func foo() async {
//    await Task.with(taskLocal: \.example, boundTo: "A") {
//      await bar(expected: "A")
//      await baz(expectedInBar: "B")
//      await bar(expected: "A")
//    }
//  }
//
//  func bar(expected: String) async {
//    let got = await Task.local(\.example)
//    assert(got == expected)
//  }
//
//  func baz(expectedInBar: String) async {
//    await Task.local(\.example, boundTo: "B") {
//      let got = await Task.local(\.example)
//      assert(got == "B")
//      await bar(expected: expectedInBar)
//    }
//  }
//}
