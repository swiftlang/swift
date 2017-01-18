// RUN: rm -rf %t && mkdir -p %t

// RUN: %swift -target x86_64-unknown-darwin14 -o %t/mips-template.swiftmodule -parse-stdlib -emit-module -module-name mips %S/../Inputs/empty.swift
// RUN: %S/Inputs/binary_sub.py x86_64-unknown-darwin14 mips64-unknown-darwin14 < %t/mips-template.swiftmodule > %t/mips.swiftmodule
// RUN: not %target-swift-frontend -I %t -typecheck -parse-stdlib %s -DMIPS 2>&1 | %FileCheck -check-prefix=CHECK-MIPS %s

// RUN: %swift -target x86_64-unknown-darwin14 -o %t/solaris-template.swiftmodule -parse-stdlib -emit-module -module-name solaris %S/../Inputs/empty.swift
// RUN: %S/Inputs/binary_sub.py x86_64-unknown-darwin14 x86_64-unknown-solaris8 < %t/solaris-template.swiftmodule > %t/solaris.swiftmodule
// RUN: not %target-swift-frontend -I %t -typecheck -parse-stdlib %s -DSOLARIS 2>&1 | %FileCheck -check-prefix=CHECK-SOLARIS %s

#if MIPS
// CHECK-MIPS: :[[@LINE+1]]:8: error: module file was created for incompatible target mips64-unknown-darwin14: {{.*}}mips.swiftmodule{{$}}
import mips

#elseif SOLARIS
// CHECK-SOLARIS: :[[@LINE+1]]:8: error: module file was created for incompatible target x86_64-unknown-solaris8: {{.*}}solaris.swiftmodule{{$}}
import solaris

#endif
