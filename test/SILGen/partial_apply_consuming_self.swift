// RUN: %target-swift-emit-silgen %s | %FileCheck %s

class C {
  __consuming func consumesSelf() {}
}

func referencesConsumesSelf(_ c: C) {
  _ = C.consumesSelf
  C.consumesSelf(c)()
}

// The method...

// CHECK-LABEL: sil hidden [ossa] @$s28partial_apply_consuming_self1CC12consumesSelfyyF : $@convention(method) (@owned C) -> () {

// The curry thunk's outer closure...

// CHECK-LABEL: sil private [ossa] @$s28partial_apply_consuming_self22referencesConsumesSelfyyAA1CCFyycADncfu_ : $@convention(thin) (@owned C) -> @owned @callee_guaranteed () -> () {

// The curry thunk's inner closure...

// CHECK-LABEL: sil private [ossa] @$s28partial_apply_consuming_self22referencesConsumesSelfyyAA1CCFyycADncfu_yycfu0_ : $@convention(thin) (@guaranteed C) -> () {