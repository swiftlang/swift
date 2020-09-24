// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -parse-stdlib -o %t -module-name someModule -module-link-name module %S/../Inputs/empty.swift
// RUN: %target-swift-frontend -disable-autolinking-runtime-compatibility-dynamic-replacements -runtime-compatibility-version none -emit-ir -lmagic %s -I %t > %t/out.txt
// RUN: %FileCheck %s < %t/out.txt
// RUN: %FileCheck -check-prefix=NO-FORCE-LOAD %s < %t/out.txt

// RUN: %empty-directory(%t/someModule.framework/Modules/someModule.swiftmodule)
// RUN: mv %t/someModule.swiftmodule %t/someModule.framework/Modules/someModule.swiftmodule/%target-swiftmodule-name
// RUN: %target-swift-frontend -disable-autolinking-runtime-compatibility-dynamic-replacements -runtime-compatibility-version none -emit-ir -lmagic %s -F %t > %t/framework.txt
// RUN: %FileCheck -check-prefix=FRAMEWORK %s < %t/framework.txt
// RUN: %FileCheck -check-prefix=NO-FORCE-LOAD %s < %t/framework.txt

// RUN: %target-swift-frontend -emit-module -parse-stdlib -o %t -module-name someModule -module-link-name module %S/../Inputs/empty.swift -autolink-force-load
// RUN: %target-swift-frontend -runtime-compatibility-version none -emit-ir -lmagic %s -I %t > %t/force-load.txt
// RUN: %FileCheck %s < %t/force-load.txt
// RUN: %FileCheck -check-prefix FORCE-LOAD-CLIENT -check-prefix FORCE-LOAD-CLIENT-%target-object-format %s < %t/force-load.txt

// RUN: %target-swift-frontend -runtime-compatibility-version none -emit-ir -debugger-support %s -I %t > %t/force-load.txt
// RUN: %FileCheck -check-prefix NO-FORCE-LOAD-CLIENT %s < %t/force-load.txt

// RUN: %target-swift-frontend -disable-autolinking-runtime-compatibility-dynamic-replacements -runtime-compatibility-version none -emit-ir -parse-stdlib -module-name someModule -module-link-name module %S/../Inputs/empty.swift | %FileCheck --check-prefix=NO-FORCE-LOAD %s
// RUN: %target-swift-frontend -runtime-compatibility-version none -emit-ir -parse-stdlib -module-name someModule -module-link-name module %S/../Inputs/empty.swift -autolink-force-load | %FileCheck --check-prefix=FORCE-LOAD %s
// RUN: %target-swift-frontend -runtime-compatibility-version none -emit-ir -parse-stdlib -module-name someModule -module-link-name 0module %S/../Inputs/empty.swift -autolink-force-load | %FileCheck --check-prefix=FORCE-LOAD-HEX %s

// Linux uses a different autolinking mechanism, based on
// swift-autolink-extract. This file tests the Darwin mechanism.
// UNSUPPORTED: autolink-extract

import someModule

// CHECK: !llvm.linker.options = !{
// CHECK-DAG: !{{[0-9]+}} = !{!{{"-lmagic"|"/DEFAULTLIB:magic.lib"}}}
// CHECK-DAG: !{{[0-9]+}} = !{!{{"-lmodule"|"/DEFAULTLIB:module.lib"}}}

// FRAMEWORK: !llvm.linker.options = !{
// FRAMEWORK-DAG: !{{[0-9]+}} = !{!{{"-lmagic"|"/DEFAULTLIB:magic.lib"}}}
// FRAMEWORK-DAG: !{{[0-9]+}} = !{!{{"-lmodule"|"/DEFAULTLIB:module.lib"}}}
// FRAMEWORK-DAG: !{{[0-9]+}} = !{!"-framework", !"someModule"}

// NO-FORCE-LOAD-NOT: FORCE_LOAD
// NO-FORCE-LOAD-NOT -lmodule
// NO-FORCE-LOAD-NOT -lmagic
// FORCE-LOAD: define{{( dllexport)?}} void @"_swift_FORCE_LOAD_$_module"() {{(#[0-9]+)?( comdat)?}} {
// FORCE-LOAD:   ret void
// FORCE-LOAD: }
// FORCE-LOAD-HEX: define{{( dllexport)?}} void @"_swift_FORCE_LOAD_$306d6f64756c65"() {{(#[0-9]+)?( comdat)?}} {
// FORCE-LOAD-HEX:   ret void
// FORCE-LOAD-HEX: }

// NO-FORCE-LOAD-CLIENT-NOT: FORCE_LOAD
// FORCE-LOAD-CLIENT: @"_swift_FORCE_LOAD_$_module_$_autolinking" = weak_odr hidden constant void ()* @"_swift_FORCE_LOAD_$_module"
// FORCE-LOAD-CLIENT: @llvm.used = appending global [{{[0-9]+}} x i8*] [
// FORCE-LOAD-CLIENT: i8* bitcast (void ()** @"_swift_FORCE_LOAD_$_module_$_autolinking" to i8*)
// FORCE-LOAD-CLIENT: ], section "llvm.metadata"
// FORCE-LOAD-CLIENT-MACHO: declare extern_weak {{(dllimport )?}}void @"_swift_FORCE_LOAD_$_module"()
// FORCE-LOAD-CLIENT-COFF: declare extern {{(dllimport )?}}void @"_swift_FORCE_LOAD_$_module"()

