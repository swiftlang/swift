// RUN: %swift -target x86_64-apple-macosx10.10 -emit-ir -disable-legacy-type-info -parse-stdlib -primary-file %s -module-name main -o - | %FileCheck %s -check-prefix CHECK-MACHO
// RUN: %swift -target x86_64-unknown-linux-gnu -emit-ir -disable-legacy-type-info -parse-stdlib -primary-file %s -module-name main -o - | %FileCheck %s -check-prefix CHECK-ELF
// RUN: %swift -target x86_64-unknown-windows-itanium -emit-ir -disable-legacy-type-info -parse-stdlib -primary-file %s -module-name main -o - | %FileCheck %s -check-prefix CHECK-COFF

// CHECK-MACHO: @"$s4main1sVMn" = constant {{.*}}, section "__TEXT,__constg_swiftt"
// CHECK-ELF: @"$s4main1sVMn" = {{.*}}constant {{.*}}, section ".rodata"
// CHECK-COFF: @"$s4main1sVMn" = {{.*}}constant {{.*}}, section ".rdata"

public struct s {
}

