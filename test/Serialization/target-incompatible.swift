// RUN: rm -rf %t && mkdir -p %t
// RUN: not %swift -I %S/Inputs -parse %s -DMIPS 2>&1 | FileCheck -check-prefix=CHECK-MIPS %s
// RUN: not %swift -I %S/Inputs -parse %s -DSOLARIS 2>&1 | FileCheck -check-prefix=CHECK-SOLARIS %s

#if MIPS
// CHECK-MIPS: :[[@LINE+1]]:8: error: module file was created for incompatible target mips64-unknown-darwin14: {{.*}}mips.swiftmodule{{$}}
import mips

#elseif SOLARIS
// CHECK-SOLARIS: :[[@LINE+1]]:8: error: module file was created for incompatible target x86_64-unknown-solaris8: {{.*}}solaris.swiftmodule{{$}}
import solaris

#endif
