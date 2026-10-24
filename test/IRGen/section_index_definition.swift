// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s
// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name m -primary-file %t/def.swift %t/use.swift -o - | %FileCheck %s --check-prefix=CHECK-DEF
// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name m -primary-file %t/use.swift %t/def.swift -o - | %FileCheck %s --check-prefix=CHECK-USE
// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name m -primary-file %t/funcs.swift -o - | %FileCheck %s --check-prefix=CHECK-FUNCS

// REQUIRES: OS_BINFMT=elf || OS_BINFMT=pe-coff

// The index describes definitions: the object file that defines the variable
// carries the entry, an object file that only uses it does not, and functions
// placed in a section are not indexed.

// BEGIN def.swift
#if objectFormat(COFF)
@section(".myplugs$B") @used public var record: Int = 1
#else
@section("myplugins") @used public var record: Int = 1
#endif

// CHECK-DEF: @__swift5_sections_entry_{{.*}}myplug
// CHECK-DEF: @llvm.used = appending global {{.*}}@__swift5_sections_entry_

// BEGIN use.swift
public func readRecord() -> Int { return record }

// CHECK-USE-NOT: __swift5_sections_
// CHECK-USE: define {{.*}}readRecord
// CHECK-USE-NOT: __swift5_sections_

// BEGIN funcs.swift
#if objectFormat(COFF)
@section(".mycode$B") @used public func placed() {}
#else
@section("mycode") @used public func placed() {}
#endif

// CHECK-FUNCS-NOT: __swift5_sections_
// CHECK-FUNCS: define {{.*}}placed{{.*}} section "{{.*}}mycode
// CHECK-FUNCS-NOT: __swift5_sections_
