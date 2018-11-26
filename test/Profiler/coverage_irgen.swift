// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -profile-generate -profile-coverage-mapping -emit-sil -o - -module-name=irgen | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -profile-generate -profile-coverage-mapping -emit-ir -o - -module-name=irgen | %FileCheck %s --check-prefix=IR

// IR-NOT: __llvm_coverage_names
// IR-NOT: __profn

// SIL: sil hidden @$s5irgen2f1yyF
// SIL: string_literal utf8 "{{.*}}coverage_irgen.swift:{{.*}}$s5irgen2f1yyF"
internal func f1() {}

// SIL: sil private @$s5irgen2f2[[F2HASH:[_a-zA-Z0-9]+]]
// SIL: string_literal utf8 "{{.*}}coverage_irgen.swift:$s5irgen2f2[[F2HASH]]"
private func f2() {}

// SIL: sil @$s5irgen2f3yyF
// SIL: string_literal utf8 "$s5irgen2f3yyF"
public func f3() {
  f1()
  f2()
}
