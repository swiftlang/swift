// RUN: %target-swift-emit-silgen %s | %FileCheck %s

class ConsumingClass {
  __consuming func consumingMethod() {}
}

// CHECK-LABEL: sil hidden [ossa] @$s21value_ownership_class14ConsumingClassC15consumingMethodyyF : $@convention(method) (@owned ConsumingClass) -> () {
