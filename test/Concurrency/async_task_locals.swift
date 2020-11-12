// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

extension Task.LocalValues {

  // TODO: can't get away with static var...
  // TODO: can't get with just a phantom type?
  public var example: ExampleKey { .init() }

  public struct ExampleKey: TaskLocalKey {
    public typealias Value = String
  }
}

func async() async {
  let thing = await Task.local(\.example)
  _ = thing
}

// ==== ------------------------------------------------------------------------

struct APIOnTaskStyle {
  func foo() async {
    await Task.withTaskLocal(\.example, boundTo: "A") {
      await bar(expected: "A")
      await baz(expectedInBar: "B")
      await bar(expected: "A")
    }
  }

  func bar(expected: String) async {
    let got = await Task.local(\.example)
    assert(got == expected)
  }

  func baz(expectedInBar: String) async {
    await Task.withTaskLocal(\.example, boundTo: "B") {
      let got = await Task.local(\.example)
      assert(got == "B")
      await bar(expected: expectedInBar)
    }
  }
}

// ==== ------------------------------------------------------------------------

struct VariableStyle {

  let exampleValue = TaskLocalValue<String>()

  // or with statically known default value, rather than defaulting to `nil`
  // let exampleValue: TaskLocalValue<String>.Defaulted =
  //   TaskLocalValue<String>.withDefault("not-set")

  func foo() async {
    await exampleValue.boundTo("A") {
      await bar(expected: "A")
      await baz(expectedInBar: "B")
      await bar(expected: "A")
    }
  }

  func bar(expected: String) async {
    let got = await exampleValue.get()
    assert(got == expected)
  }

  func baz(expectedInBar: String) async {
    await exampleValue.boundTo("B") {
      let got = await exampleValue.get()
      assert(got == "B")
      await bar(expected: expectedInBar)
    }
  }

}
