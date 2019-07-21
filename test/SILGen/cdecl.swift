// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// CHECK-LABEL: sil hidden [thunk] [ossa] @pear : $@convention(c)
// CHECK:         function_ref @$s5cdecl5apple{{[_0-9a-zA-Z]*}}F
// CHECK-LABEL: sil hidden [ossa] @$s5cdecl5apple{{[_0-9a-zA-Z]*}}F
@_cdecl("pear")
func apple(_ f: @convention(c) (Int) -> Int) {
}

// CHECK-LABEL: sil hidden [ossa] @$s5cdecl16forceCEntryPoint{{[_0-9a-zA-Z]*}}F
// CHECK:         function_ref @grapefruit
func forceCEntryPoint() {
  apple(orange)
}

// CHECK-LABEL: sil hidden [thunk] [ossa] @grapefruit : $@convention(c)
// CHECK:         function_ref @$s5cdecl6orange{{[_0-9a-zA-Z]*}}F
// CHECK-LABEL: sil hidden [ossa] @$s5cdecl6orange{{[_0-9a-zA-Z]*}}F
@_cdecl("grapefruit")
func orange(_ x: Int) -> Int {
  return x
}

// CHECK-LABEL: sil [serializable] [thunk] [ossa] @cauliflower : $@convention(c)
// CHECK:         function_ref @$s5cdecl8broccoli{{[_0-9a-zA-Z]*}}F
// CHECK-LABEL: sil [ossa] @$s5cdecl8broccoli{{[_0-9a-zA-Z]*}}F
@_cdecl("cauliflower")
public func broccoli(_ x: Int) -> Int {
  return x
}

// CHECK-LABEL: sil private [thunk] [ossa] @collard_greens : $@convention(c)
// CHECK:         function_ref @$s5cdecl4kale[[PRIVATE:.*]]
// CHECK:       sil private [ossa] @$s5cdecl4kale[[PRIVATE:.*]]
@_cdecl("collard_greens")
private func kale(_ x: Int) -> Int {
  return x
}

/* TODO: Handle error conventions
@_cdecl("vomits")
func barfs() throws {}
 */
