// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/redirects)
// RUN: split-file %s %t

// RUN: sed -e "s|OUT_DIR|%t/redirects|g"  -e "s|IN_DIR|%S/Inputs/CHeaders|g" %t/overlay_template.yaml > %t/overlay.yaml
// RUN: sed -e "s|OUT_DIR|%t/redirects|g"  -e "s|IN_DIR|%S/Inputs/CHeaders1|g" %t/overlay_template.yaml > %t/overlay1.yaml
// RUN: sed -e "s|OUT_DIR|%t/redirects|g"  -e "s|IN_DIR|%S/Inputs/CHeaders2|g" %t/overlay_template.yaml > %t/overlay2.yaml

// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -module-cache-path %t/module-cache %t/test.swift -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -file-compilation-dir %t -Xcc -ivfsoverlay -Xcc %t/overlay.yaml
// RUN: %validate-json %t/deps.json > %t/validated_deps.json
// RUN: cat %t/validated_deps.json | %FileCheck %s

// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -module-cache-path %t/module-cache %t/test.swift -o %t/deps1.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -file-compilation-dir %t -Xcc -ivfsoverlay -Xcc %t/overlay1.yaml
// RUN: %validate-json %t/deps1.json > %t/validated_deps1.json
// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -module-cache-path %t/module-cache %t/test.swift -o %t/deps2.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -experimental-clang-importer-direct-cc1-scan -file-compilation-dir %t -Xcc -ivfsoverlay -Xcc %t/overlay2.yaml
// RUN: %validate-json %t/deps2.json > %t/validated_deps2.json
// RUN: cat %t/validated_deps.json %t/validated_deps1.json %t/validated_deps2.json \
// RUN:   | %FileCheck %s --check-prefix=MOD-HASH

//--- overlay_template.yaml
{
  'version': 0,
  'use-external-names': false,
  'roots': [
    {
      'name': 'IN_DIR', 'type': 'directory',
      'contents': [
      ]
    },
  ]
}

//--- test.swift
import F

// CHECK: "mainModuleName": "deps"
/// --------Main module
// CHECK-LABEL: "modulePath": "deps.swiftmodule",
// CHECK-NEXT: sourceFiles
// CHECK-NEXT: test.swift
// CHECK-NEXT: ],
// CHECK-NEXT: "directDependencies": [
// CHECK-DAG:     "swift": "F"
// CHECK-DAG:     "swift": "Swift"
// CHECK-DAG:     "swift": "SwiftOnoneSupport"
// CHECK: ],

// Ensure that the VFS overlay command-line flag is pruned on the Swift module dependency
// that uses a Clang module which has optimized it away as un-used.
/// --------Swift module F
// CHECK-LABEL: "modulePath": "{{.*}}{{/|\\}}F-{{.*}}.swiftmodule",

// CHECK: "directDependencies": [
// CHECK-NEXT:   {
// CHECK-DAG:     "clang": "F"
// CHECK-DAG:     "swift": "Swift"
// CHECK-DAG:     "swift": "SwiftOnoneSupport"
// CHECK-NEXT:   }
// CHECK-NEXT: ],

// CHECK: "commandLine": [
// CHECK: "-compile-module-from-interface"
// CHECK-NOT: "-ivfsoverlay",
// CHECK-NOT: "{{.*}}{{/|\\}}preserve_used_vfs.swift.tmp{{/|\\}}overlay.yaml",
// CHECK-NOT: "-ffile-compilation-dir={{.*}}"
// CHECK: ],

/// --------Clang module F
// CHECK-LABEL: "modulePath": "{{.*}}{{/|\\}}F-{{.*}}.pcm",
// CHECK-NOT: "-ivfsoverlay",
// CHECK-NOT: "{{.*}}{{/|\\}}preserve_used_vfs.swift.tmp{{/|\\}}overlay.yaml",

/// Check that the dependency swift module hashes are identical when the vfs overlays are ignored.
// MOD-HASH: "mainModuleName": "deps",
// MOD-HASH: "linkLibraries": [],
// MOD-HASH-NEXT: "details": {
// MOD-HASH-NEXT:   "swift": {
// MOD-HASH-NEXT:   "moduleInterfacePath": "{{.*}}{{/|\\}}F.swiftinterface",
// MOD-HASH:   "commandLine": [
// MOD-HASH:     "-swift-module-file=Swift={{.*}}{{/|\\}}Swift-[[SHASH:.*]].swiftmodule",
// MOD-HASH:     "-swift-module-file=SwiftOnoneSupport={{.*}}{{/|\\}}SwiftOnoneSupport-[[SOSHASH:.*]].swiftmodule",
// MOD-HASH:     "-o",
// MOD-HASH-NEXT:     "{{.*}}{{/|\\}}F-[[FHASH:.*]].swiftmodule"
// MOD-HASH:     ],
// MOD-HASH: "mainModuleName": "deps1",
// MOD-HASH: "linkLibraries": [],
// MOD-HASH: "details": {
// MOD-HASH-NEXT:   "swift": {
// MOD-HASH-NEXT:   "moduleInterfacePath": "{{.*}}{{/|\\}}F.swiftinterface",
// MOD-HASH:   "commandLine": [
// MOD-HASH:     "-swift-module-file=Swift={{.*}}{{/|\\}}Swift-[[SHASH]].swiftmodule",
// MOD-HASH:     "-swift-module-file=SwiftOnoneSupport={{.*}}{{/|\\}}SwiftOnoneSupport-[[SOSHASH]].swiftmodule",
// MOD-HASH:     "-o",
// MOD-HASH-NEXT:     "{{.*}}{{/|\\}}F-[[FHASH]].swiftmodule"
// MOD-HASH:     ],
// MOD-HASH: "mainModuleName": "deps2",
// MOD-HASH: "linkLibraries": [],
// MOD-HASH: "details": {
// MOD-HASH-NEXT:   "swift": {
// MOD-HASH-NEXT:   "moduleInterfacePath": "{{.*}}{{/|\\}}F.swiftinterface",
// MOD-HASH:   "commandLine": [
// MOD-HASH:     "-swift-module-file=Swift={{.*}}{{/|\\}}Swift-[[SHASH]].swiftmodule",
// MOD-HASH:     "-swift-module-file=SwiftOnoneSupport={{.*}}{{/|\\}}SwiftOnoneSupport-[[SOSHASH]].swiftmodule",
// MOD-HASH:     "-o",
// MOD-HASH-NEXT:     "{{.*}}{{/|\\}}F-[[FHASH]].swiftmodule"
// MOD-HASH:     ],
