// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_force_emission %s | %FileCheck %s

final class VarInit {
  // coverage_force_emission.VarInit.(lazyVarInit in _7D375D72BA8B0C53C9AD7E4DBC7FF493).getter : Swift.String
  // CHECK: sil_coverage_map {{.*}} _T023coverage_force_emission7VarInitC04lazydE033_7D375D72BA8B0C53C9AD7E4DBC7FF493LLSSvg
  private lazy var lazyVarInit: String = {
    return "Hello"
  }()

  // closure #1 () -> Swift.String in variable initialization expression of coverage_force_emission.VarInit.(basicVarInit in _7D375D72BA8B0C53C9AD7E4DBC7FF493) : Swift.String
  // CHECK: sil_coverage_map {{.*}} _T023coverage_force_emission7VarInitC05basicdE033_7D375D72BA8B0C53C9AD7E4DBC7FF493LLSSvpfiSSycfU_
  private var basicVarInit: String = {
    return "Hello"
  }()

  // coverage_force_emission.VarInit.(simpleVar in _7D375D72BA8B0C53C9AD7E4DBC7FF493).getter : Swift.String
  // CHECK: sil_coverage_map {{.*}} _T023coverage_force_emission7VarInitC06simpleD033_7D375D72BA8B0C53C9AD7E4DBC7FF493LLSSvg
  private var simpleVar: String {
    return "Hello"
  }
}
