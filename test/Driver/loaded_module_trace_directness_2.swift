// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/imported_modules/ComplexModuleGraph2 %t/include

// REQUIRES: objc_interop

// Dependency graph:
//
// * CoreFilesystem - Swift module with generated ObjC header
// * FilesystemKit - ObjC module without overlay, has #import <CoreFileystem-Generated.h>
// * TestFilesystem - Swift module, has import CoreFilesystem or FilesystemKit.
//
// * CoreDaemon - ObjC module with overlay, the overlay has import DaemonKit
// * DaemonKit - ObjC module without overlay, has #import <CoreDaemon.h>
// * TestDaemon - Swift module, has import CoreDaemon or DaemonKit.
// NOTE: This mimics the Darwin -> SwiftOverlayShims -> Darwin dependency "cycle".
//
// * CoreMemory - ObjC module with overlay and generated header for overlay, the overlay has import MemoryKit
// * MemoryKit - ObjC module without overlay, has #import <CoreMemory-Generated.h>
// * TestMemory - Swift module, has import CoreMemory or MemoryKit.

// 1. CoreFilesystem - Build.

// RUN: %target-swift-frontend %s -emit-module -o %t/include/CoreFilesystem.swiftmodule -DCoreFilesystem -module-name CoreFilesystem -emit-objc-header-path %t/include/CoreFilesystem-Generated.h -disable-objc-attr-requires-foundation-module

#if CoreFilesystem
@objc
public class MyNode {
    public var number: Int
    public init(_ n: Int) { number = n }
}
#endif

// 2. FilesystemKit - Nothing to do (pure Clang module).

// 3. TestFilesystem - Check that CoreFilesystem and Filesytem can be imported.

// RUN: %target-swift-frontend %s -typecheck -DTestFilesystem -DV1 -module-name TestFilesystemV1 -emit-loaded-module-trace-path %t/TestFilesystemV1.trace.json -I %t/include
// RUN: %FileCheck %s --check-prefix=TESTFILESYSTEM < %t/TestFilesystemV1.trace.json
// RUN: %target-swift-frontend %s -typecheck -DTestFilesystem -DV2 -module-name TestFilesystemV2 -emit-loaded-module-trace-path %t/TestFilesystemV2.trace.json -I %t/include
// RUN: %FileCheck %s --check-prefix=TESTFILESYSTEM < %t/TestFilesystemV2.trace.json
// RUN: %target-swift-frontend %s -typecheck -DTestFilesystem -DV3 -module-name TestFilesystemV3 -emit-loaded-module-trace-path %t/TestFilesystemV3.trace.json -I %t/include
// RUN: %FileCheck %s --check-prefix=TESTFILESYSTEM < %t/TestFilesystemV3.trace.json
// RUN: %target-swift-frontend %s -typecheck -DTestFilesystem -DV4 -module-name TestFilesystemV4 -emit-loaded-module-trace-path %t/TestFilesystemV4.trace.json -I %t/include
// RUN: %FileCheck %s --check-prefix=TESTFILESYSTEM < %t/TestFilesystemV4.trace.json

#if TestFilesystem
  #if V1
    import CoreFilesystem
  #endif
  #if V2
    import FilesystemKit
    public func noop(_: CoreFilesystem.MyNode) {}
  #endif
  #if V3
    import CoreFilesystem
    import FilesystemKit
  #endif
  #if V4
    import FilesystemKit
    import CoreFilesystem
  #endif
#endif

// FilesystemKit has no overlay, so it shouldn't show up.
// TESTFILESYSTEM: "swiftmodulesDetailedInfo":[
// TESTFILESYSTEM-NOT: FilesystemKit
// TESTFILESYSTEM-DAG: {"name":"CoreFilesystem","path":"{{[^"]*}}CoreFilesystem.swiftmodule","isImportedDirectly":true,
// TESTFILESYSTEM: ]

// 4. CoreDaemon - Build.

// RUN: %target-swift-frontend %s -emit-module -o %t/include/CoreDaemon.swiftmodule -DCoreDaemon -module-name CoreDaemon -emit-loaded-module-trace-path %t/CoreDaemon.trace.json -I %t/include
// RUN: %FileCheck %s --check-prefix=COREDAEMON < %t/CoreDaemon.trace.json

// * CoreDaemon - ObjC module with overlay, the overlay has import DaemonKit
// * DaemonKit - ObjC module without overlay, has #import <CoreDaemon.h>
// * TestDaemon - Swift module, has import CoreDaemon or DaemonKit.

#if CoreDaemon
@_exported import CoreDaemon
import DaemonKit

public func runBoth(_ pair: DaemonKit.DaemonPair) {
    let daemons : (CoreDaemon.Daemon, CoreDaemon.Daemon) = pair.daemons;
    daemons.0.run(0);
    daemons.1.run(1);
}
#endif

// COREDAEMON: "swiftmodulesDetailedInfo":[
// COREDAEMON-NOT: CoreDaemon
// COREDAEMON-NOT: DaemonKit
// COREDAEMON: ]

// 5. DaemonKit - Nothing to do (pure Clang module).

// 6. TestCoreDaemon

// RUN: %target-swift-frontend %s -typecheck -DTestDaemon -DV1 -module-name TestDaemonV1 -emit-loaded-module-trace-path %t/TestDaemonV1.trace.json -I %t/include
// RUN: %FileCheck %s --check-prefix=TESTDAEMON < %t/TestDaemonV1.trace.json

// RUN: %target-swift-frontend %s -typecheck -DTestDaemon -DV2 -module-name TestDaemonV2 -emit-loaded-module-trace-path %t/TestDaemonV2.trace.json -I %t/include
// RUN: %FileCheck %s --check-prefix=TESTDAEMON < %t/TestDaemonV2.trace.json

// RUN: %target-swift-frontend %s -typecheck -DTestDaemon -DV3 -module-name TestDaemonV3 -emit-loaded-module-trace-path %t/TestDaemonV3.trace.json -I %t/include
// RUN: %FileCheck %s --check-prefix=TESTDAEMON < %t/TestDaemonV3.trace.json

// RUN: %target-swift-frontend %s -typecheck -DTestDaemon -DV4 -module-name TestDaemonV4 -emit-loaded-module-trace-path %t/TestDaemonV4.trace.json -I %t/include
// RUN: %FileCheck %s --check-prefix=TESTDAEMON < %t/TestDaemonV4.trace.json

#if TestDaemon
  #if V1
    import CoreDaemon
  #endif
  #if V2
    import DaemonKit
    public func noop(_: CoreDaemon.Daemon, _: MaxwellsDaemon) {}
  #endif
  #if V3
    import CoreDaemon
    import DaemonKit
  #endif
  #if V4
    import DaemonKit
    import CoreDaemon
  #endif
#endif

// DaemonKit has no overlay, so it shouldn't show up.
// TESTDAEMON: "swiftmodulesDetailedInfo":[
// TESTDAEMON-NOT: DaemonKit
// TESTDAEMON-DAG: {"name":"CoreDaemon","path":"{{[^"]*}}CoreDaemon.swiftmodule","isImportedDirectly":true,
// TESTDAEMON: ]

// 7. CoreMemory - Build.

// RUN: %target-swift-frontend %s -emit-module -o %t/include/CoreMemory.swiftmodule -DCoreMemory -module-name CoreMemory -emit-objc-header-path %t/include/CoreMemory-Generated.h -disable-objc-attr-requires-foundation-module -I %t/include

#if CoreMemory
@_exported import CoreMemory

@objc
public class MemoryMapEntry {
    public var region: MemoryMapRegion
    public var permissions: Int = 0
    public init(_ r: MemoryMapRegion) { region = r }
}
#endif

// 8. MemoryKit - Nothing to do (pure Clang module).

// 9. TestMemory - Check that CoreMemory and MemoryKit can be imported.

// RUN: %target-swift-frontend %s -typecheck -DTestMemory -DV1 -module-name TestMemoryV1 -emit-loaded-module-trace-path %t/TestMemoryV1.trace.json -I %t/include
// RUN: %FileCheck %s --check-prefix=TESTMEMORY < %t/TestMemoryV1.trace.json
// RUN: %target-swift-frontend %s -typecheck -DTestMemory -DV2 -module-name TestMemoryV2 -emit-loaded-module-trace-path %t/TestMemoryV2.trace.json -I %t/include
// RUN: %FileCheck %s --check-prefix=TESTMEMORY < %t/TestMemoryV2.trace.json
// RUN: %target-swift-frontend %s -typecheck -DTestMemory -DV3 -module-name TestMemoryV3 -emit-loaded-module-trace-path %t/TestMemoryV3.trace.json -I %t/include
// RUN: %FileCheck %s --check-prefix=TESTMEMORY < %t/TestMemoryV3.trace.json
// RUN: %target-swift-frontend %s -typecheck -DTestMemory -DV4 -module-name TestMemoryV4 -emit-loaded-module-trace-path %t/TestMemoryV4.trace.json -I %t/include
// RUN: %FileCheck %s --check-prefix=TESTMEMORY < %t/TestMemoryV4.trace.json

#if TestMemory
  #if V1
    import CoreMemory
  #endif
  #if V2
    import MemoryKit
    public func noop(_: CoreMemory.MemoryMapRegion, _: CoreMemory.MemoryMapEntry) {}
  #endif
  #if V3
    import CoreMemory
    import MemoryKit
  #endif
  #if V4
    import MemoryKit
    import CoreMemory
  #endif
#endif

// MemoryKit has no overlay, so it shouldn't show up.
// TESTMEMORY: "swiftmodulesDetailedInfo":[
// TESTMEMORY-NOT: MemoryKit
// TESTMEMORY-DAG: {"name":"CoreMemory","path":"{{[^"]*}}CoreMemory.swiftmodule","isImportedDirectly":true,
// TESTMEMORY: ]
