// This test tests that conditionally live records
// are preserved through the pipeline until the linker,
// at which point even linkers not with no special
// support for conditionally live symbols discard the
// records during dead stripping

// REQUIRES: system-darwin

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -use-ld=ld64 -Osize -emit-library -Xcc -Xclang -Xcc -opaque-pointers -Xfrontend -resilient-conditional-runtime-records -lto=llvm-full -Xlinker -map -Xlinker %t/link-map.txt -Xlinker -save-temps -Xlinker -dead_strip -save-temps %s -o %t/libTest.dylib
// RUN: %llvm-nm -j --defined-only %t/libTest.dylib.lto.o | %FileCheck %s --check-prefix=PRE-LINK
// RUN: cat %t/link-map.txt | head -n $(grep -n "# Dead Stripped Symbols:" %t/link-map.txt | cut -f1 -d:) | tail -n "+$(grep -n '# Symbols:' %t/link-map.txt | cut -f1 -d:)" | %FileCheck %s --check-prefix=POST-LINK

// PRE-LINK:___$s4Test8Protocol_pMF_cl
// PRE-LINK: l___$s4Test8ProtocolHr_cl
// POST-LINK-NOT: l___$s4Test8ProtocolHr_cl
// POST-LINK-NOT: ___$s4Test8Protocol_pMF_cl
public protocol Protocol {}
