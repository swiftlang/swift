// RUN: %empty-directory(%t)

// Test WMO supplementary output functionality

// Test SIL consolidates, IR separates in multi-threaded WMO
// RUN: %target-swift-frontend -wmo -num-threads 4 %S/../Driver/Inputs/main.swift %s -module-name=ThisModule -c -o %t/main.o -o %t/multi-threaded.o -sil-output-path %t/mt-wmo.sil -ir-output-path %t/main.ll -ir-output-path %t/multi-threaded.ll
// RUN: test -f %t/mt-wmo.sil && test -f %t/main.ll && test -f %t/multi-threaded.ll && test -f %t/main.o && test -f %t/multi-threaded.o
// RUN: %FileCheck -input-file %t/mt-wmo.sil %s --check-prefix=SIL-CHECK
// RUN: %FileCheck -input-file %t/main.ll %s --check-prefix=IR-CHECK-MAIN
// RUN: %FileCheck -input-file %t/multi-threaded.ll %s --check-prefix=IR-CHECK

// MARK: Single-threaded WMO tests - Both SIL and IR consolidate

// Test single-threaded WMO: both SIL and IR produce consolidated output
// RUN: %target-swift-frontend -wmo %S/../Driver/Inputs/main.swift %s -module-name=ThisModule -c -o %t/st-main.o -sil-output-path %t/st-wmo.sil -ir-output-path %t/st-wmo.ll
// RUN: test -f %t/st-wmo.sil && test -f %t/st-wmo.ll && test -f %t/st-main.o
// RUN: %FileCheck -input-file %t/st-wmo.sil %s --check-prefix=SIL-CHECK
// RUN: %FileCheck -input-file %t/st-wmo.ll %s --check-prefix=IR-CHECK

// MARK: WMO with supplementary output file maps - First entry consolidation

// Test file map consolidation: both SIL and IR use first entry naming with consolidated content
// RUN: echo '{"%/S/../Driver/Inputs/main.swift": {"sil": "%/t/map.sil", "llvm-ir": "%/t/map.ll"}, "%/s": {"sil": "%/t/unused.sil", "llvm-ir": "%/t/unused.ll"}}' > %t/map.json
// RUN: %target-swift-frontend -wmo %/S/../Driver/Inputs/main.swift %/s -module-name=ThisModule -c -o %t/map.o -supplementary-output-file-map %t/map.json
// RUN: test -f %t/map.sil && test -f %t/map.ll && test -f %t/map.o
// RUN: test ! -f %t/unused.sil && test ! -f %t/unused.ll
// RUN: %FileCheck -input-file %t/map.sil %s --check-prefix=SIL-CHECK
// RUN: %FileCheck -input-file %t/map.ll %s --check-prefix=IR-CHECK

// SIL-CHECK: sil {{.*}} @$s10ThisModule15libraryFunctionyyF

// IR-CHECK: @"$s10ThisModule15libraryFunctionyyF"

// IR-CHECK-MAIN: define{{.*}} i32 @main(

func libraryFunction() {}
