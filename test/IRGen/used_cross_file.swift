// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -primary-file %t/use.swift %t/def.swift -module-name M -parse-as-library -emit-ir -o - | %FileCheck %s --check-prefix=USE
// RUN: %target-swift-frontend %t/use.swift -primary-file %t/def.swift -module-name M -parse-as-library -emit-ir -o - | %FileCheck %s --check-prefix=DEF

// A file that references a '@used' variable or function defined in another
// file of the same module must not list the declaration in llvm.used; only
// the file that emits the definition does. Listing a declaration tripped the
// "all globals in the 'used' list must be definitions" assertion.

//--- def.swift
@used public var record: Int = 1
@used public func retained() {}

//--- use.swift
public func readRecord() -> Int { record }
public func callRetained() { retained() }

// USE: @llvm.used = appending global
// USE-NOT: @"$s1M6recordSivp",
// USE-NOT: @"$s1M6recordSivp"]
// USE-NOT: @"$s1M6recordSivau",
// USE-NOT: @"$s1M6recordSivau"]
// USE-NOT: @"$s1M8retainedyyF",
// USE-NOT: @"$s1M8retainedyyF"]
// USE: declare swiftcc {{.*}}@"$s1M6recordSivau"

// DEF: @llvm.used = appending global [{{[0-9]+}} x ptr] [{{.*(\$s1M6recordSivp.*\$s1M8retainedyyF|\$s1M8retainedyyF.*\$s1M6recordSivp).*}}], section "llvm.metadata"
