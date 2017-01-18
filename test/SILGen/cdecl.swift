// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

// CHECK-LABEL: sil hidden [thunk] @pear : $@convention(c)
// CHECK:         function_ref @_TF5cdecl5apple
// CHECK-LABEL: sil hidden @_TF5cdecl5apple
@_cdecl("pear")
func apple(_ f: @convention(c) (Int) -> Int) {
}

// CHECK-LABEL: sil hidden @_TF5cdecl16forceCEntryPoint
// CHECK:         function_ref @grapefruit
func forceCEntryPoint() {
  apple(orange)
}

// CHECK-LABEL: sil hidden [thunk] @grapefruit : $@convention(c)
// CHECK:         function_ref @_TF5cdecl6orange
// CHECK-LABEL: sil hidden @_TF5cdecl6orange
@_cdecl("grapefruit")
func orange(_ x: Int) -> Int {
  return x
}

// CHECK-LABEL: sil [thunk] @cauliflower : $@convention(c)
// CHECK:         function_ref @_TF5cdecl8broccoli
// CHECK-LABEL: sil @_TF5cdecl8broccoli
@_cdecl("cauliflower")
public func broccoli(_ x: Int) -> Int {
  return x
}

// CHECK-LABEL: sil private [thunk] @collard_greens : $@convention(c)
// CHECK:         function_ref @_TF5cdeclP[[PRIVATE:.*]]4kale
// CHECK:       sil private @_TF5cdeclP[[PRIVATE:.*]]4kale
@_cdecl("collard_greens")
private func kale(_ x: Int) -> Int {
  return x
}

/* TODO: Handle error conventions
@_cdecl("vomits")
func barfs() throws {}
 */
