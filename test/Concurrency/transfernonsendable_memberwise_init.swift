// RUN: %target-swift-frontend -emit-sil -language-mode 5 -strict-concurrency=complete -target %target-swift-5.1-abi-triple -verify %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -language-mode 6 -target %target-swift-5.1-abi-triple -verify %s | %FileCheck %s

// REQUIRES: concurrency

@MainActor
@propertyWrapper
struct Observed<T> {
  var wrappedValue: T
  init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
}

@MainActor
protocol MyView {}

class MyModel {}

struct TestView: MyView {
  @Observed var model: MyModel

  // CHECK: // TestView.init(model:)
  // CHECK-NEXT: // Isolation: global_actor. type: MainActor
  // CHECK-NEXT: sil hidden @$s35transfernonsendable_memberwise_init8TestViewV5modelAcA7MyModelC_tcfC : $@convention(method) (@owned MyModel, @thin TestView.Type) -> @owned TestView
}

@MainActor
func test() {
  _ = TestView(model: MyModel()) // Ok
}
