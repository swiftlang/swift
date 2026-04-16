// RUN: %target-swift-frontend -emit-sil -language-mode 6 -disable-availability-checking -verify %s | %FileCheck %s

////////////////////////
// MARK: Declarations //
////////////////////////

class KlassNonsendable {}

// Inner closure in this example gets specialized and it's important to make sure that it gets the right isolation
// and the reference to `ns` doesn't get diagnosed.
func testCaptureWithClosureSpecialization() {
  @MainActor func test(_: @escaping @MainActor () async -> Void) async {}

  @MainActor
  class Test {
    func compute() async {
      var ns: KlassNonsendable? = .init()
      // expected-warning@-1 {{variable 'ns' was never mutated; consider changing to 'let' constant}}

      await test {
        if let ns {
          // CHECK: // compute() in Test #1 in testCaptureWithClosureSpecialization()
          // CHECK-NEXT: // Isolation: global_actor. type: MainActor
          // CHECK-LABEL: sil private @$s56transfernonsendable_closure_captures_with_specialization36testCaptureWithClosureSpecializationyyF4TestL_C7computeyyYaF : $@convention(method) @async (@guaranteed Test) -> () {
          // CHECK: [[NS:%.*]] = enum $Optional<KlassNonsendable>, #Optional.some!enumelt, {{.*}}
          // CHECK: [[SPECIALIZED_CLOSURE:%.*]] = function_ref @$s56transfernonsendable_closure_captures_with_specialization36testCaptureWithClosureSpecializationyyF4TestL_C7computeyyYaFyyYaYbScMYccfU_Tf2in_n : $@convention(thin) @Sendable @async (@guaranteed Optional<KlassNonsendable>, @thick @dynamic_self Test.Type) -> ()
          // CHECK-NEXT: partial_apply [callee_guaranteed] [[SPECIALIZED_CLOSURE]]([[NS]], {{.*}})
          // CHECK: } // end sil function '$s56transfernonsendable_closure_captures_with_specialization36testCaptureWithClosureSpecializationyyF4TestL_C7computeyyYaF'
          Task {
            await Self.takesNS(ns)
          }
        }
      }
    }

    static func takesNS(_: KlassNonsendable) async {}
  }
}

// CHECK: // specialized closure #1 in compute() in Test #1 in testCaptureWithClosureSpecialization()
// CHECK-NEXT: // Isolation: global_actor. type: MainActor
// CHECK-NEXT: sil private @$s56transfernonsendable_closure_captures_with_specialization36testCaptureWithClosureSpecializationyyF4TestL_C7computeyyYaFyyYaYbScMYccfU_Tf2in_n : $@convention(thin) @Sendable @async (@guaranteed Optional<KlassNonsendable>, @thick @dynamic_self Test.Type) -> ()
