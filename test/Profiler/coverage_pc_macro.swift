// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -pc-macro -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_pc_macro %s | %FileCheck %s
// RUN: %target-swift-frontend -pc-macro -profile-generate -profile-coverage-mapping -emit-ir %s

// Make sure the PCMacro transform doesn't cause coverage to crash.

public func __builtin_pc_before(_ sl : Int, _ el : Int, _ sc : Int, _ ec: Int, _ moduleId : Int, _ fileId : Int) {
  print("[\(moduleId):\(fileId)] [\(sl):\(sc)-\(el):\(ec)] pc before")
}

public func __builtin_pc_after(_ sl : Int, _ el : Int, _ sc : Int, _ ec: Int, _ moduleId : Int, _ fileId : Int) {
  print("[\(moduleId):\(fileId)] [\(sl):\(sc)-\(el):\(ec)] pc after")
}

// CHECK-LABEL: sil_coverage_map {{.*}} "$s17coverage_pc_macro3fooySiSbF"
func foo(_ x: Bool) -> Int {
  if x {
    return 5
  } else {
    return 7
  }
}
