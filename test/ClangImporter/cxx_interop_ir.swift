// RUN: %target-swift-frontend -module-name cxx_ir -I %S/Inputs/custom-modules -module-cache-path %t -enable-cxx-interop -emit-ir -o - -primary-file %s | %FileCheck %s

import CXXInterop

// CHECK-LABEL: define hidden swiftcc void @"$s6cxx_ir13indirectUsageyyF"()
// CHECK: %0 = call %"class.ns::T"* @{{_Z5makeTv|"\?makeT@@YAPE?AVT@ns@@XZ"}}()
// CHECK: call void @{{_Z4useTPN2ns1TE|"\?useT@@YAXPE?AVT@ns@@@Z"}}(%"class.ns::T"* %2)
func indirectUsage() {
  useT(makeT())
}

// CHECK: define hidden swiftcc void @"$s6cxx_ir24namespaceManglesIntoName3argySo2nsV1TV_tF"
func namespaceManglesIntoName(arg: namespacedT) {
}
