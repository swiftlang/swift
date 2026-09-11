// RUN: %target-swift-frontend -package-name Package -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -package-name Package -emit-sil %s -o /dev/null -verify

public class Base {
  package init(value: Int) {}
}

class Subclass: Base {
  let name: String

  init(name: String) {
    self.name = name
    super.init(value: 0)
  }
}

// CHECK-LABEL: // vtable thunk for Base.__allocating_init(value:) dispatching to Subclass.__allocating_init(value:)
// CHECK-NOT: class_method
// CHECK: function_ref @$s{{.*}}8SubclassC5valueACSi_tcfC
