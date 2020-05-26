// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-pch -index-store-path %t/idx -o %t/bridge-head.pch %S/Inputs/bridge-head.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -import-objc-header %t/bridge-head.pch -primary-file %s -o %t/s1.o -index-store-path %t/idx
// RUN: c-index-test core -print-record %t/idx | %FileCheck %s --check-prefix=PCH-RECORD
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s --check-prefix=PCH-UNIT
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -import-objc-header %S/Inputs/bridge-head.h -primary-file %s -o %t/s1.o -index-store-path %t/idx2
// RUN: c-index-test core -print-unit %t/idx2 | %FileCheck --check-prefix=TEXTUAL-UNIT %s

// PCH-RECORD: bridge-include.h
// PCH-RECORD: ------------
// PCH-RECORD: function/C | includedFunc | {{.*}} | <no-cgname> | Decl - 
// PCH-RECORD: variable/C | MY_CONST | {{.*}} | <no-cgname> | Def - 
// PCH-RECORD: ------------
// PCH-RECORD: 1:5 | function/C | {{.*}} | Decl | rel: 0
// PCH-RECORD: 2:5 | variable/C | {{.*}} | Def | rel: 0

// PCH-UNIT: bridge-head.pch-
// PCH-UNIT: --------
// PCH-UNIT: has-main: 0
// PCH-UNIT: DEPEND START
// PCH-UNIT: Record | user | {{.*}}bridge-include.h | bridge-include.h-
// PCH-UNIT: File | user | {{.*}}bridge-head.h
// PCH-UNIT: File | user | {{.*}}module.modulemap
// PCH-UNIT: DEPEND END (3)
// PCH-UNIT: INCLUDE START
// PCH-UNIT: {{.*}}bridge-head.h:1 | {{.*}}bridge-include.h
// PCH-UNIT: INCLUDE END (1)

// PCH-UNIT: s1.o-
// PCH-UNIT: --------
// PCH-UNIT: has-main: 1
// PCH-UNIT: DEPEND START
// PCH-UNIT: Unit | system | {{.*}}Swift.swiftmodule
// PCH-UNIT: Unit | user | {{.*}}bridge-head.pch | bridge-head.pch-
// PCH-UNIT: Record | user | {{.*}}unit-with-bridging-header.swift | unit-with-bridging-header.swift-
// PCH-UNIT: DEPEND END (3)

// TEXTUAL-UNIT: s1.o-
// TEXTUAL-UNIT: --------
// TEXTUAL-UNIT: has-main: 1
// TEXTUAL-UNIT: DEPEND START
// TEXTUAL-UNIT: Unit | system | {{.*}}Swift.swiftmodule
// TEXTUAL-UNIT: Record | user | {{.*}}unit-with-bridging-header.swift | unit-with-bridging-header.swift-
// TEXTUAL-UNIT: DEPEND END (2)

func test() {}
