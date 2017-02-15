// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -profile-generate -profile-coverage-mapping -emit-sil -o - | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -profile-generate -profile-coverage-mapping -emit-ir -o - | %FileCheck %s --check-prefix=IR

// IR-NOT: __llvm_coverage_names

// SIL-DAG: sil hidden @_TF8coverage2f1FT_T_
// SIL-DAG: string_literal utf8 "{{.*}}coverage.swift:{{.*}}_TF8coverage2f1FT_T_"
// IR-DAG: @"__profn_{{.*}}coverage.swift:_TF8coverage2f1FT_T_" {{.*}} c"{{.*}}coverage.swift:_TF8coverage2f1FT_T_"
internal func f1() {}

// SIL-DAG: sil private @_TF8coverageP33_[[F2HASH:[_a-zA-Z0-9]+]]
// SIL-DAG: string_literal utf8 "{{.*}}coverage.swift:_TF8coverageP33_[[F2HASH]]"
// IR-DAG: @"__profn_{{.*}}coverage.swift:_TF8coverageP33_[[F2HASH:[_a-zA-Z0-9]+]]" {{.*}} c"{{.*}}coverage.swift:_TF8coverageP33_[[F2HASH]]"
private func f2() {}

// SIL-DAG: sil @_TF8coverage2f3FT_T_
// SIL-DAG: string_literal utf8 "_TF8coverage2f3FT_T_"
// IR-DAG: @__profn__TF8coverage2f3FT_T_ {{.*}} c"_TF8coverage2f3FT_T_"
public func f3() {
  f1();
  f2();
}
