// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/inputs)
// RUN: %empty-directory(%t/inputs-2)
// RUN: split-file %s %t

// RUN: sed -e "s|OUT_DIR|%t/redirects|g"  -e "s|IN_DIR|%t/inputs|g" %t/overlay_template.yaml > %t/overlay.yaml
// RUN: sed -e "s|OUT_DIR|%t/redirects-2|g"  -e "s|IN_DIR|%t/inputs-2|g" %t/overlay_template_2.yaml > %t/overlay-2.yaml

/// Put some files in RealFileSystem that need to be over shadow by VFS.
// RUN: touch %t/inputs/module.modulemap
// RUN: touch %t/inputs-2/module.modulemap

// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-serialized -module-cache-path %t/module-cache %t/test.swift -o %t/deps.json -I %t/inputs -I %t/inputs-2 -I %S/Inputs/Swift -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -Xcc -ivfsoverlay -Xcc %t/overlay.yaml -Xcc -ivfsoverlay -Xcc %t/overlay-2.yaml
// RUN: %validate-json %t/deps.json | %FileCheck %s

// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json Swift > %t/swift.cmd
// RUN: %swift_frontend_plain @%t/swift.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json SwiftOnoneSupport > %t/onone.cmd
// RUN: %swift_frontend_plain @%t/onone.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:Indirect > %t/Indirect.cmd
// RUN: %swift_frontend_plain @%t/Indirect.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:F > %t/F.cmd
// RUN: %swift_frontend_plain @%t/F.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json F > %t/SwiftF.cmd
// RUN: %swift_frontend_plain @%t/SwiftF.cmd

// RUN: %{python} %S/../CAS/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-swift-modules\"" >> %t/MyApp.cmd
// RUN: echo "\"-explicit-swift-module-map-file\"" >> %t/MyApp.cmd
// RUN: echo "\"%t/map.json\"" >> %t/MyApp.cmd

// RUN: %target-swift-frontend @%t/MyApp.cmd %t/test.swift -Xcc -ivfsoverlay -Xcc %t/overlay.yaml -Xcc -ivfsoverlay -Xcc %t/overlay-2.yaml \
// RUN:   -emit-module -o %t/Test.swiftmodule

//--- redirects/RedirectedF.h
#include "Indirect_2.h"
void funcRedirectedF(void);

//--- redirects/modulemap
module F {
  header "F_2.h"
  export *
}

//--- redirects-2/RedirectedIndirect.h
void funcRedirectedIndirect(void);

//--- redirects-2/modulemap
module Indirect {
  header "Indirect_2.h"
  export *
}

//--- overlay_template.yaml
{
  'version': 0,
  'use-external-names': false,
  'roots': [
    {
      'name': 'IN_DIR', 'type': 'directory',
      'contents': [
        { 'name': 'F_2.h', 'type': 'file',
          'external-contents': 'OUT_DIR/RedirectedF.h'
        },
        { 'name': 'module.modulemap', 'type': 'file',
          'external-contents': 'OUT_DIR/modulemap'
        }
      ]
    },
  ]
}

//--- overlay_template_2.yaml
{
  'version': 0,
  'use-external-names': false,
  'roots': [
    {
      'name': 'IN_DIR', 'type': 'directory',
      'contents': [
        { 'name': 'Indirect_2.h', 'type': 'file',
          'external-contents': 'OUT_DIR/RedirectedIndirect.h'
        },
        { 'name': 'module.modulemap', 'type': 'file',
          'external-contents': 'OUT_DIR/modulemap'
        }
      ]
    },
  ]
}

//--- test.swift
import F

func testF() { funcRedirectedF() }
func testIndirect() { funcRedirectedIndirect() }

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

// Ensure that the VFS overlay command-line flag is preserved on the Swift module dependency
// that uses a Clang module affected by this overlay
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
// CHECK: "-ivfsoverlay",
// CHECK-NEXT: "-Xcc",
// CHECK-NEXT: "{{.*}}{{/|\\}}preserve_used_vfs.swift.tmp{{/|\\}}overlay.yaml",
// CHECK-NEXT: "-Xcc",
// CHECK-NEXT: "-ivfsoverlay",
// CHECK-NEXT: "-Xcc",
// CHECK-NEXT: "{{.*}}{{/|\\}}preserve_used_vfs.swift.tmp{{/|\\}}overlay-2.yaml",
// CHECK: ],

/// --------Clang module F
// CHECK-LABEL: "modulePath": "{{.*}}{{/|\\}}F-{{.*}}.pcm",
// CHECK: "commandLine": [
// CHECK: "-vfsoverlay",
// CHECK-NEXT: "{{.*}}{{/|\\}}preserve_used_vfs.swift.tmp{{/|\\}}overlay.yaml",
// CHECK-NEXT: "-vfsoverlay",
// CHECK-NEXT: "{{.*}}{{/|\\}}preserve_used_vfs.swift.tmp{{/|\\}}overlay-2.yaml",
// CHECK: "-ivfsoverlay",
// CHECK-NEXT: "-Xcc",
// CHECK-NEXT: "{{.*}}{{/|\\}}preserve_used_vfs.swift.tmp{{/|\\}}overlay.yaml",
// CHECK-NEXT: "-Xcc",
// CHECK-NEXT: "-ivfsoverlay",
// CHECK-NEXT: "-Xcc",
// CHECK-NEXT: "{{.*}}{{/|\\}}preserve_used_vfs.swift.tmp{{/|\\}}overlay-2.yaml",
// CHECK: ]

/// --------Clang module Indirect
// CHECK-LABEL: "modulePath": "{{.*}}{{/|\\}}Indirect-{{.*}}.pcm",
// CHECK-NOT: overlay.yaml
// CHECK: "-vfsoverlay",
// CHECK-NEXT: "{{.*}}{{/|\\}}preserve_used_vfs.swift.tmp{{/|\\}}overlay-2.yaml",
// CHECK: "-ivfsoverlay",
// CHECK-NEXT: "-Xcc",
// CHECK-NEXT: "{{.*}}{{/|\\}}preserve_used_vfs.swift.tmp{{/|\\}}overlay-2.yaml",
