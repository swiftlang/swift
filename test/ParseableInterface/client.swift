// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/modulecache)
//
// Test will build a module TestModule that depends on OtherModule and LeafModule (built from other.swift and leaf.swift).
//
// RUN: echo 'public func LeafFunc() -> Int { return 10; }' >%t/leaf.swift
//
// RUN: echo 'import LeafModule' >%t/other.swift
// RUN: echo 'public func OtherFunc() -> Int { return LeafFunc(); }' >>%t/other.swift
//
// Both inputs are initially set to modification-time 201401240005. Subsequent derived files will be
// touched to have timestamps 1 minute forward each (i.e. minutes 06, 07, 08)
//
// RUN: touch -t 201401240005 %t/leaf.swift
// RUN: touch -t 201401240005 %t/other.swift
//
//
// Phase 1: build LeafModule into a .swiftinterface file:
//
// RUN: %target-swift-frontend -I %t -emit-parseable-module-interface-path %t/LeafModule.swiftinterface -module-name LeafModule %t/leaf.swift -emit-module -o /dev/null
// RUN: test -f %t/LeafModule.swiftinterface
// RUN: %FileCheck %s -check-prefix=CHECK-LEAFINTERFACE <%t/LeafModule.swiftinterface
// CHECK-LEAFINTERFACE: LeafFunc
// RUN: touch -t 201401240006 %t/LeafModule.swiftinterface
//
//
// Phase 2: build OtherModule into a .swiftinterface _using_ LeafModule via LeafModule.swiftinterface, creating LeafModule-*.swiftmodule along the way.
//
// RUN: %target-swift-frontend -I %t -module-cache-path %t/modulecache -enable-parseable-module-interface -emit-parseable-module-interface-path %t/OtherModule.swiftinterface -module-name OtherModule %t/other.swift -emit-module -o /dev/null
// RUN: test -f %t/OtherModule.swiftinterface
// RUN: %FileCheck %s -check-prefix=CHECK-OTHERINTERFACE <%t/OtherModule.swiftinterface
// CHECK-OTHERINTERFACE: OtherFunc
// RUN: test -f %t/modulecache/LeafModule-*.swiftmodule
// RUN: llvm-bcanalyzer -dump %t/modulecache/LeafModule-*.swiftmodule | %FileCheck %s -check-prefix=CHECK-LEAFMODULE
// CHECK-LEAFMODULE: {{MODULE_NAME.*blob data = 'LeafModule'}}
// CHECK-LEAFMODULE: {{FILE_DEPENDENCY.*Swift.swiftmodule'}}
// CHECK-LEAFMODULE: {{FILE_DEPENDENCY.*SwiftOnoneSupport.swiftmodule'}}
// CHECK-LEAFMODULE: {{FILE_DEPENDENCY.*LeafModule.swiftinterface'}}
// CHECK-LEAFMODULE: FUNC_DECL
// RUN: touch -t 201401240007 %t/modulecache/LeafModule-*.swiftmodule
// RUN: touch -t 201401240007 %t/OtherModule.swiftinterface
//
//
// Phase 3: build TestModule into a .swiftmodule explicitly us OtherModule via OtherModule.swiftinterface, creating OtherModule-*.swiftmodule along the way.
//
// RUN: %target-swift-frontend -I %t -module-cache-path %t/modulecache -enable-parseable-module-interface -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %s
// RUN: test -f %t/TestModule.swiftmodule
// RUN: test -f %t/modulecache/OtherModule-*.swiftmodule
// RUN: llvm-bcanalyzer -dump %t/modulecache/OtherModule-*.swiftmodule | %FileCheck %s -check-prefix=CHECK-OTHERMODULE
// CHECK-OTHERMODULE: {{MODULE_NAME.*blob data = 'OtherModule'}}
// CHECK-OTHERMODULE: {{FILE_DEPENDENCY.*Swift.swiftmodule'}}
// CHECK-OTHERMODULE: {{FILE_DEPENDENCY.*SwiftOnoneSupport.swiftmodule'}}
// CHECK-OTHERMODULE: {{FILE_DEPENDENCY.*LeafModule-.*.swiftmodule'}}
// CHECK-OTHERMODULE: {{FILE_DEPENDENCY.*OtherModule.swiftinterface'}}
// CHECK-OTHERMODULE: FUNC_DECL
// RUN: touch -t 201401240008 %t/modulecache/OtherModule-*.swiftmodule
// RUN: touch -t 201401240008 %t/TestModule.swiftmodule
//
//
// Phase 4: Same command as in phase 3, but check that none of the cached modules are rebuilt.
// RUN: %target-swift-frontend -I %t -module-cache-path %t/modulecache -enable-parseable-module-interface -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %s
// RUN: touch -t 201401240008 %t/TestModule.swiftmodule
// RUN: test ! %t/TestModule.swiftmodule -ot %t/modulecache/OtherModule-*.swiftmodule
// RUN: test ! %t/TestModule.swiftmodule -ot %t/modulecache/LeafModule-*.swiftmodule
//
//
// Phase 5: change the mtime on LeafModule.swiftinterface and watch LeafModule-*.swiftmodule and OtherModule-*.swiftmodule recompile.
// RUN: touch -t 201401240016 %t/LeafModule.swiftinterface
// RUN: %target-swift-frontend -I %t -module-cache-path %t/modulecache -enable-parseable-module-interface -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %s
// RUN: test %t/LeafModule.swiftinterface -ot %t/modulecache/LeafModule-*.swiftmodule
// RUN: test %t/LeafModule.swiftinterface -ot %t/modulecache/OtherModule-*.swiftmodule
//
//
// Phase 6: change the mtimes on LeafModule-*.swiftmodule and OtherModule.swiftinterface, and watch just OtherModule-*.swiftmodule recompile, leaving LeafModule-*.swiftmodule alone.
// RUN: touch -t 201401240016 %t/modulecache/LeafModule-*.swiftmodule
// RUN: touch -t 201401240017 %t/OtherModule.swiftinterface
// RUN: %target-swift-frontend -I %t -module-cache-path %t/modulecache -enable-parseable-module-interface -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %s
// RUN: test ! %t/OtherModule.swiftinterface -ot %t/modulecache/LeafModule-*.swiftmodule
// RUN: test %t/OtherModule.swiftinterface -ot %t/modulecache/OtherModule-*.swiftmodule
//
//
// Phase 7: change the size on LeafModule.swiftinterface (keeping mtime fixed) and watch LeafModule-*.swiftmodule and OtherModule-*.swiftmodule recompile.
// RUN: touch -t 201401240017 %t/modulecache/OtherModule-*.swiftmodule
// RUN: echo '// size change' >>%t/LeafModule.swiftinterface
// RUN: touch -t 201401240016 %t/LeafModule.swiftinterface
// RUN: %target-swift-frontend -I %t -module-cache-path %t/modulecache -enable-parseable-module-interface -emit-module -o %t/TestModule.swiftmodule -module-name TestModule %s
// RUN: test %t/LeafModule.swiftinterface -ot %t/modulecache/LeafModule-*.swiftmodule
// RUN: test %t/LeafModule.swiftinterface -ot %t/modulecache/OtherModule-*.swiftmodule

import OtherModule

public func TestFunc() {
    print(OtherFunc())
}
