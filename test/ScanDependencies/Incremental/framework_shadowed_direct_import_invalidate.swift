// REQUIRES: objc_interop

// Check the framework case of a newly introduced dependency that shadows an existing one. 

// Test.swift imports ModB directly, and also transitively through SwiftDep -> ModA -> ModB. 
// This test checks through different scenarios of ModB showing up in different search paths.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/BuildDir)

// RUN: split-file %s %t

// Initial scan: ModB resolves to the Frameworks search path.
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/test.swift -o %t/deps_initial.json -module-name Test -I %t/SwiftDeps -F %t/BuildDir -F %t/Frameworks -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import
// RUN: %validate-json %t/deps_initial.json | %FileCheck %s -check-prefix=INITIAL

// Add ModB to BuildDir.
// Rescan: ModB now resolves to the earlier search path BuildDir, invalidating ModB + dependents.
// RUN: cp -R %t/BuildDirVariants/ModB.framework %t/BuildDir/ModB.framework
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %t/test.swift -o %t/deps_rescan.json -module-name Test -I %t/SwiftDeps -F %t/BuildDir -F %t/Frameworks -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -validate-prior-dependency-scan-cache &> %t/rescan_output.txt
// RUN: cat %t/rescan_output.txt | %FileCheck %s -check-prefix=RESCAN-OUTPUT
// RUN: %validate-json %t/deps_rescan.json | %FileCheck %s -check-prefix=RESCAN

// Scan without a dependency scan cache: same results as the rescan. 
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -module-cache-path %t/module-cache %t/test.swift -o %t/deps_clean.json -module-name Test -I %t/SwiftDeps -F %t/BuildDir -F %t/Frameworks -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import
// RUN: diff %t/deps_rescan.json %t/deps_clean.json

// INITIAL: "moduleMapPath": "{{.*}}Frameworks{{/|\\\\}}ModA.framework{{/|\\\\}}Modules{{/|\\\\}}module.modulemap"
// INITIAL-NOT: BuildDir{{/|\\\\}}ModB.framework
// INITIAL: "moduleMapPath": "{{.*}}Frameworks{{/|\\\\}}ModB.framework{{/|\\\\}}Modules{{/|\\\\}}module.modulemap"

// RESCAN-OUTPUT: remark: Incremental module scan: Re-using serialized module scanning dependency cache from: {{.*}}cache.moddepcache
// RESCAN-OUTPUT: remark: Incremental module scan: Dependency info for module 'ModB' invalidated because the module was previously defined in '{{.*}}Frameworks{{[/\\]}}ModB.framework{{[/\\]}}Modules{{[/\\]}}module.modulemap' but is now found in '{{.*}}BuildDir{{[/\\]}}ModB.framework{{[/\\]}}Modules{{[/\\]}}module.modulemap'.

// ModA's build command must use the same ModB as Test does.
// RESCAN: "moduleMapPath": "{{.*}}Frameworks{{/|\\\\}}ModA.framework{{/|\\\\}}Modules{{/|\\\\}}module.modulemap"
// RESCAN-NOT: Frameworks{{/|\\\\}}ModB.framework
// RESCAN: "-fmodule-map-file={{.*}}BuildDir{{/|\\\\}}ModB.framework{{/|\\\\}}Modules{{/|\\\\}}module.modulemap"
// RESCAN: "moduleMapPath": "{{.*}}BuildDir{{/|\\\\}}ModB.framework{{/|\\\\}}Modules{{/|\\\\}}module.modulemap"

//--- test.swift
import SwiftDep
import ModB

//--- SwiftDeps/SwiftDep.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name SwiftDep
import ModA

//--- Frameworks/ModA.framework/Headers/ModA.h
#import <ModB/ModB.h>

//--- Frameworks/ModA.framework/Modules/module.modulemap
framework module ModA {
  header "ModA.h"
  export *
}

//--- Frameworks/ModB.framework/Headers/ModB.h
// ModB in 'Frameworks'

//--- Frameworks/ModB.framework/Modules/module.modulemap
framework module ModB {
  header "ModB.h"
  export *
}

//--- BuildDirVariants/ModB.framework/Headers/ModB.h
// ModB in 'BuildDir'

//--- BuildDirVariants/ModB.framework/Modules/module.modulemap
framework module ModB {
  header "ModB.h"
  export *
}
