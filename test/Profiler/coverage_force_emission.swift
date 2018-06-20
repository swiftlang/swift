// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sil -module-name coverage_force_emission %s | %FileCheck %s -check-prefix=COVERAGE
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -emit-sil -module-name coverage_force_emission %s | %FileCheck %s -check-prefix=PGO

final class VarInit {
  // COVERAGE: sil_coverage_map {{.*}} "$S23coverage_force_emission7VarInitC04lazydE033_7D375D72BA8B0C53C9AD7E4DBC7FF493LLSSvgSSyXEfU_"
  // PGO-LABEL: coverage_force_emission.VarInit.(lazyVarInit in _7D375D72BA8B0C53C9AD7E4DBC7FF493).getter : Swift.String
  // PGO: int_instrprof_increment
  private lazy var lazyVarInit: String = {
    return "Hello"
  }()

  // CHECK: sil_coverage_map {{.*}} "$S23coverage_force_emission7VarInitC05basicdE033_7D375D72BA8B0C53C9AD7E4DBC7FF493LLSSvpfiSSyXEfU_"
  // PGO-LABEL: closure #1 () -> Swift.String in variable initialization expression of coverage_force_emission.VarInit.(basicVarInit in _7D375D72BA8B0C53C9AD7E4DBC7FF493) : Swift.String
  // PGO: int_instrprof_increment
  private var basicVarInit: String = {
    return "Hello"
  }()

  // CHECK: sil_coverage_map {{.*}} "$S23coverage_force_emission7VarInitC06simpleD033_7D375D72BA8B0C53C9AD7E4DBC7FF493LLSSvg"
  // PGO-LABEL: coverage_force_emission.VarInit.(simpleVar in _7D375D72BA8B0C53C9AD7E4DBC7FF493).getter : Swift.String
  // PGO: int_instrprof_increment
  private var simpleVar: String {
    return "Hello"
  }
}
