// RUN: %target-swift-emit-silgen -enable-sil-ownership %s

// Capture list with weak capture vs noescape closure
func transform<T>(fn: () -> T) -> T {
  return fn()
}

// Make sure weak and unowned are captured by box, even from a noescape closure.

class Bar {
  var x: Int = 27

  // CHECK-LABEL: sil hidden @$s12capture_list3BarC4testyyF : $@convention(method) (@guaranteed Bar) -> ()
  // CHECK: [[FN:%.*]] = function_ref @$s12capture_list9transformxxyc2fn_tlF : $@convention(thin) <τ_0_0> (@owned @callee_owned () -> @out τ_0_0) -> @out τ_0_0
  // CHECK: [[RESULT:%.*]] = alloc_stack $Int
  // CHECK: [[BOX:%.*]] = alloc_box ${ var @sil_weak Optional<Bar> }
  // CHECK: [[PAYLOAD:%.*]] = project_box [[BOX]]
  // CHECK: [[COPY:%.*]] = copy_value %0
  // CHECK: [[OPTIONAL_COPY:%.*]] = enum $Optional<Bar>, #Optional.some!enumelt.1
  // CHECK: store_weak [[OPTIONAL_COPY]] to [initialization] [[PAYLOAD]]
  // CHECK: destroy_value [[OPTIONAL_COPY]]
  // CHECK: [[CLOSURE_FN:%.*]] = function_ref @$s12capture_list3BarC4testyyFSiycfU_ : $@convention(thin) (@owned { var @sil_weak Optional<Bar> }) -> Int
  // CHECK: [[BOX_COPY:%.*]] = copy_value [[BOX]]
  // CHECK: [[CLOSURE:%.*]] = partial_apply [[CLOSURE_FN]]([[BOX_COPY]])
  // CHECK: [[THUNK_FN:%.*]] = function_ref @$sSiIxd_SiIxr_TR
  // CHECK: [[THUNK:%.*]] = partial_apply [[THUNK_FN]]([[CLOSURE]]])
  // CHECK: destroy_value [[BOX]]
  // CHECK: apply [[FN]]([[THUNK]])
  // CHECK: return
  func test() {
    transform { [weak self] in
      return self!.x
    }
  }
}


// Capture list vs autoclosure.

func block(_ f: () -> Void) -> Int { return 42 }
func oneOf(_ a: Int?, _ b: @autoclosure () -> Int) -> Int { return 0 }
class Foo {
    private var value: Int?
    func refresh() {
        _ = oneOf(self.value, block({
            [unowned self] in _ = self
        }))
    }
}
