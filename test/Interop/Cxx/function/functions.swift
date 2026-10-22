// RUN: %target-swift-frontend %s -emit-irgen -I %S/Inputs -o - -cxx-interoperability-mode=default | %IRGenFileCheck %s

import Functions

// CHECK-LABEL: define{{.*}} i{{16|32}} @{{.*}}getWChar
func test0() -> CWideChar {
  return getWChar()
}
