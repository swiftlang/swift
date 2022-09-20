// RUN: %empty-directory(%t)

// RUN: %swift -target x86_64-unknown-darwin14 -o %t/mips-template.swiftmodule -parse-stdlib -emit-module -module-name mips -Xllvm -sil-disable-pass=target-constant-folding %S/../Inputs/empty.swift
// RUN: %{python} -u %S/Inputs/binary_sub.py x86_64-unknown-darwin14 mips64-unknown-darwin14 < %t/mips-template.swiftmodule > %t/mips.swiftmodule
// RUN: not %target-swift-frontend -I %t -typecheck -parse-stdlib %s -DMIPS 2>&1 | %FileCheck -check-prefix=CHECK-MIPS %s

// RUN: %swift -target x86_64-unknown-darwin14 -o %t/solaris-template.swiftmodule -parse-stdlib -emit-module -module-name solaris -Xllvm -sil-disable-pass=target-constant-folding %S/../Inputs/empty.swift
// RUN: %{python} -u %S/Inputs/binary_sub.py x86_64-unknown-darwin14 x86_64-unknown-solaris8 < %t/solaris-template.swiftmodule > %t/solaris.swiftmodule
// RUN: not %target-swift-frontend -I %t -typecheck -parse-stdlib %s -DSOLARIS 2>&1 | %FileCheck -check-prefix=CHECK-SOLARIS %s

// Check that we still get the diagnostic but the module is output anyway when
// allowing errors
// RUN: %target-swift-frontend -I %t -parse-stdlib -experimental-allow-module-with-compiler-errors -emit-module -module-name incompatmips -o %t %s -DMIPS 2>&1 | %FileCheck -check-prefix=CHECK-MIPS %s
// RUN: ls %t/incompatmips.swiftmodule
// RUN: %target-swift-frontend -I %t -parse-stdlib -experimental-allow-module-with-compiler-errors -emit-module -module-name incompatsol -o %t %s -DSOLARIS 2>&1 | %FileCheck -check-prefix=CHECK-SOLARIS %s
// RUN: ls %t/incompatsol.swiftmodule

// These checks should still hold with -enable-library-evolution.

// RUN: %swift -target x86_64-unknown-darwin14 -o %t/mips-template.swiftmodule -parse-stdlib -emit-module -module-name mips -Xllvm -sil-disable-pass=target-constant-folding %S/../Inputs/empty.swift -enable-library-evolution
// RUN: %{python} -u %S/Inputs/binary_sub.py x86_64-unknown-darwin14 mips64-unknown-darwin14 < %t/mips-template.swiftmodule > %t/mips.swiftmodule
// RUN: not %target-swift-frontend -I %t -typecheck -parse-stdlib %s -DMIPS 2>&1 | %FileCheck -check-prefix=CHECK-MIPS %s

// RUN: %swift -target x86_64-unknown-darwin14 -o %t/solaris-template.swiftmodule -parse-stdlib -emit-module -module-name solaris -Xllvm -sil-disable-pass=target-constant-folding %S/../Inputs/empty.swift -enable-library-evolution
// RUN: %{python} -u %S/Inputs/binary_sub.py x86_64-unknown-darwin14 x86_64-unknown-solaris8 < %t/solaris-template.swiftmodule > %t/solaris.swiftmodule
// RUN: not %target-swift-frontend -I %t -typecheck -parse-stdlib %s -DSOLARIS 2>&1 | %FileCheck -check-prefix=CHECK-SOLARIS %s

#if MIPS
// CHECK-MIPS: :[[@LINE+1]]:8: error: module 'mips' was created for incompatible target mips64-unknown-darwin14: {{.*}}mips.swiftmodule{{$}}
import mips

#elseif SOLARIS
// CHECK-SOLARIS: :[[@LINE+1]]:8: error: module 'solaris' was created for incompatible target x86_64-unknown-solaris8: {{.*}}solaris.swiftmodule{{$}}
import solaris

#endif
