// RUN: %target-swiftxx-frontend -emit-irgen %s -I %S/Inputs | %FileCheck %s

// Make sure Swift handles the C++ pointer-to-implementation idiom properly.

import PIMPL

// Trigger type metadata to be emitted by conforming C++ types to a Swift protocol.
protocol MyProto {}
extension HasPIMPL : MyProto {}
extension HasSmartPIMPL : MyProto {}

let _ = createHasPIMPL()
let _ = createHasSmartPIMPL()

// CHECK-NOT: @"get_type_metadata {{.*}}default_delete{{.*}}
