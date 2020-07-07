// %s expands to an absolute path, so to test relative paths we need to create a
// clean directory, put the source there, and cd into it.
// RUN: rm -rf %t
// RUN: mkdir -p %t/foo/bar/baz
// RUN: echo "func coverage() {}" > %t/foo/bar/baz/coverage_relative_path.swift
// RUN: cd %t/foo/bar

// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -Xllvm -enable-name-compression=false -emit-ir %/t/foo/bar/baz/coverage_relative_path.swift | %FileCheck -check-prefix=ABSOLUTE %s
//
// ABSOLUTE: @__llvm_coverage_mapping = {{.*"\\01.*foo.*bar.*baz.*coverage_relative_path\.swift}}

// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -Xllvm -enable-name-compression=false -coverage-prefix-map %/t/foo/bar=. -emit-ir %/t/foo/bar/baz/coverage_relative_path.swift | %FileCheck -check-prefix=RELATIVE %s
//
// RELATIVE: @__llvm_coverage_mapping = {{.*"\\01[^/]*}}.{{/|\\}}baz{{.*coverage_relative_path\.swift}}

func coverage() {}
