/// Regression test for the Clang dependency scanner losing ClangImporter's
/// in-memory file system.
///
/// On non-Darwin targets `ClangImporter` injects a system VFS overlay
/// (`<resource-dir>/<clang-system-vfs-overlay>`) that redirects the platform
/// libc `module.modulemap` at the Swift-provided one. That YAML file exists
/// *only* in the `InMemoryFileSystem` built by
/// `ClangImporter::computeClangImporterFileSystem`, yet it is handed to Clang
/// as a plain `-ivfsoverlay` argument on the scanner command line.
///
/// If the Clang scanning workers' base file system does not contain that
/// in-memory file, `createVFSFromOverlayFiles` cannot open it, reports
/// `err_missing_vfs_overlay_file` and skips it. Clang then ends up with fewer
/// `RedirectingFileSystem`s than `HeaderSearchOpts::VFSOverlayFiles` entries,
/// and `HeaderSearch::collectVFSUsageAndClear` asserts while the scanner writes
/// a PCM:
///
///   Assertion `VFSUsage.size() == getHeaderSearchOpts().VFSOverlayFiles.size()
///   && "A different number of RedirectingFileSystem's were present than
///   -ivfsoverlay options passed to Clang!"' failed.
///
/// The target is pinned to Linux so that this reproduces from any host; no
/// target stdlib is needed because of `-parse-stdlib`.
///
/// rdar://184810704

// REQUIRES: asserts

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// A fake Linux sysroot: enough libc headers for
/// `getLibcFileMapping`'s `findFirstIncludeDir` probe, and a Swift-provided
/// glibc.modulemap for it to redirect `usr/include/module.modulemap` to.
// RUN: mkdir -p %t/sdk/usr/include
// RUN: touch %t/sdk/usr/include/inttypes.h %t/sdk/usr/include/unistd.h %t/sdk/usr/include/stdint.h
// RUN: mkdir -p %t/sdk/usr/lib/swift/linux/x86_64
// RUN: cp %t/glibc.modulemap %t/sdk/usr/lib/swift/linux/x86_64/glibc.modulemap
// RUN: touch %t/sdk/usr/lib/swift/linux/x86_64/SwiftGlibc.h

/// Sanity check: the scanner command line really does carry the in-memory-only
/// overlay. Without this the test could silently stop covering the bug.
// RUN: %swift_frontend_plain -frontend -scan-dependencies -parse-stdlib \
// RUN:   -target x86_64-unknown-linux-gnu -sdk %t/sdk -module-name repro \
// RUN:   -Xcc -I%t/include -module-cache-path %t/mcp \
// RUN:   -dump-clang-diagnostics %t/main.swift -o %t/deps.json 2>&1 \
// RUN:   | %FileCheck --check-prefix=ARGS %s
// ARGS: clang importer driver args:
// ARGS-SAME: '-ivfsoverlay' '{{.*}}<clang-system-vfs-overlay>'

/// The scan itself must not crash, and must still resolve the Clang module.
/// Scanning `CRepro` makes the scanner write a PCM, which is where
/// `collectVFSUsageAndClear` runs.
// RUN: %swift_frontend_plain -frontend -scan-dependencies -parse-stdlib \
// RUN:   -target x86_64-unknown-linux-gnu -sdk %t/sdk -module-name repro \
// RUN:   -Xcc -I%t/include -module-cache-path %t/mcp \
// RUN:   %t/main.swift -o %t/deps.json
// RUN: %FileCheck --check-prefix=DEPS %s < %t/deps.json
// DEPS: "clang": "CRepro"

//--- glibc.modulemap
module SwiftGlibc [system] {
  header "SwiftGlibc.h"
  export *
}

//--- include/module.modulemap
module CRepro {
  header "CRepro.h"
  export *
}

//--- include/CRepro.h
int repro(void);

//--- main.swift
import CRepro
