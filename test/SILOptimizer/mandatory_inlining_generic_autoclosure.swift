// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

@_transparent func takesGenericAutoclosure<T>(_ fn: @autoclosure () -> T) {
  _ = fn()
}

// CHECK-LABEL: sil hidden @$s38mandatory_inlining_generic_autoclosure23callsGenericAutoclosureyyxlF : $@convention(thin) <T> (@in_guaranteed T) -> () {
// FIXME
// CHE/CK-NOT: function_ref @$s38mandatory_inlining_generic_autoclosure23callsGenericAutoclosureyyxlFxyXEfu_

func callsGenericAutoclosure<T>(_ t: T) {
  takesGenericAutoclosure(t)
}
