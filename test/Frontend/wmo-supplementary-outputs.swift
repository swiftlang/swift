// RUN: %empty-directory(%t)

// Test WMO supplementary output functionality

// Check if SIL extra output works with WMO multi-threaded mode
// WMO should produce one SIL file containing the entire module
// RUN: %target-swift-frontend -wmo -num-threads 4 %S/../Driver/Inputs/main.swift %s -module-name=ThisModule -c -o %t/main.o -o %t/multi-threaded.o -sil-output-path %t/wmo.sil
// RUN: test -f %t/wmo.sil && test -f %t/main.o && test -f %t/multi-threaded.o
// RUN: %FileCheck -input-file %t/wmo.sil %s --check-prefix=SIL-CHECK

// Check if LLVM IR extra output works with WMO multi-threaded mode
// WMO should produce separate IR files for each source file
// RUN: %target-swift-frontend -wmo -num-threads 4 %S/../Driver/Inputs/main.swift %s -module-name=ThisModule -c -o %t/main.o -o %t/multi-threaded.o -ir-output-path %t/wmo.ll
// RUN: test -f %t/wmo.ll && test -f %t/main.o && test -f %t/multi-threaded.o
// RUN: %FileCheck -input-file %t/wmo.ll %s --check-prefix=IR-CHECK

// Check if multiple SIL output paths work with WMO multi-threaded mode
// With WMO, the SIL path matching the module name should be used if available
// RUN: %target-swift-frontend -wmo -num-threads 4 %S/../Driver/Inputs/main.swift %s -module-name=ThisModule -c -o %t/main.o -o %t/multi-threaded.o -sil-output-path %t/first.sil -sil-output-path %t/ThisModule.sil
// RUN: test -f %t/ThisModule.sil && test -f %t/main.o && test -f %t/multi-threaded.o
// RUN: test ! -f %t/first.sil
// RUN: %FileCheck -input-file %t/ThisModule.sil %s --check-prefix=SIL-CHECK

// Check fallback behaviour when no SIL path matches module name
// Should use the first SIL path when module name doesn't match any path
// RUN: %target-swift-frontend -wmo -num-threads 4 %S/../Driver/Inputs/main.swift %s -module-name=ThisModule -c -o %t/main2.o -o %t/multi-threaded2.o -sil-output-path %t/fallback.sil -sil-output-path %t/other.sil
// RUN: test -f %t/fallback.sil && test -f %t/main2.o && test -f %t/multi-threaded2.o
// RUN: test ! -f %t/other.sil
// RUN: %FileCheck -input-file %t/fallback.sil %s --check-prefix=SIL-CHECK

// Check if multiple IR output paths work with WMO multi-threaded mode
// IR generation creates separate modules per source file in WMO mode
// RUN: %target-swift-frontend -wmo -num-threads 4 %S/../Driver/Inputs/main.swift %s -module-name=ThisModule -c -o %t/main.o -o %t/multi-threaded.o -ir-output-path %t/main.ll -ir-output-path %t/multi-threaded.ll
// RUN: test -f %t/main.ll && test -f %t/multi-threaded.ll && test -f %t/main.o && test -f %t/multi-threaded.o
// RUN: %FileCheck -input-file %t/main.ll %s --check-prefix=IR-CHECK-MAIN
// RUN: %FileCheck -input-file %t/multi-threaded.ll %s --check-prefix=IR-CHECK

// Check if combined SIL and IR output paths work with WMO multi-threaded mode
// RUN: %target-swift-frontend -wmo -num-threads 4 %S/../Driver/Inputs/main.swift %s -module-name=ThisModule -c -o %t/main.o -o %t/multi-threaded.o -sil-output-path %t/combined.sil -ir-output-path %t/combined_main.ll -ir-output-path %t/combined_multi.ll
// RUN: test -f %t/combined.sil && test -f %t/combined_main.ll && test -f %t/combined_multi.ll && test -f %t/main.o && test -f %t/multi-threaded.o
// RUN: %FileCheck -input-file %t/combined.sil %s --check-prefix=SIL-CHECK
// RUN: %FileCheck -input-file %t/combined_main.ll %s --check-prefix=IR-CHECK-MAIN
// RUN: %FileCheck -input-file %t/combined_multi.ll %s --check-prefix=IR-CHECK

// SIL-CHECK: sil {{.*}} @$s10ThisModule15libraryFunctionyyF

// IR-CHECK: @"$s10ThisModule15libraryFunctionyyF"

// IR-CHECK-MAIN: define{{.*}} i32 @main(

func libraryFunction() {}
