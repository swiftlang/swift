// REQUIRES: swift_swift_parser
// UNSUPPORTED: OS=windows-msvc

/// Test that the scanner passes the in-process plugin server path when a
/// library plugin is used, and that the path is prefix mapped so the cache
/// key doesn't depend on the toolchain location.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/plugins)
// RUN: %empty-directory(%t/toolchain-a)
// RUN: %empty-directory(%t/toolchain-b)
// RUN: split-file %s %t

// RUN: %host-build-swift \
// RUN:   -swift-version 5 \
// RUN:   -emit-library \
// RUN:   -o %t/plugins/%target-library-name(MacroDefinition) \
// RUN:   -module-name=MacroDefinition \
// RUN:   %S/../Macros/Inputs/syntax_macro_definitions.swift \
// RUN:   -g -no-toolchain-stdlib-rpath

/// Make the in-process plugin server available from two toolchain locations.
// RUN: ln -s %swift-lib-dir %t/toolchain-a/lib
// RUN: ln -s %swift-lib-dir %t/toolchain-b/lib

// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-serialized -module-name MyApp -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/macro.swift -o %t/deps-a.json -swift-version 5 -cache-compile-job -cas-path %t/cas \
// RUN:   -load-plugin-library %t/plugins/%target-library-name(MacroDefinition) \
// RUN:   -in-process-plugin-server-path %t/toolchain-a/lib/swift/host/%target-library-name(SwiftInProcPluginServer) \
// RUN:   -scanner-prefix-map-paths %t/toolchain-a /^toolchain -resolved-plugin-verification

// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-serialized -module-name MyApp -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/macro.swift -o %t/deps-b.json -swift-version 5 -cache-compile-job -cas-path %t/cas \
// RUN:   -load-plugin-library %t/plugins/%target-library-name(MacroDefinition) \
// RUN:   -in-process-plugin-server-path %t/toolchain-b/lib/swift/host/%target-library-name(SwiftInProcPluginServer) \
// RUN:   -scanner-prefix-map-paths %t/toolchain-b /^toolchain -resolved-plugin-verification

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps-a.json -o %t/MyApp-a.cmd
// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps-b.json -o %t/MyApp-b.cmd

// RUN: %FileCheck %s --check-prefix=CMD --input-file=%t/MyApp-a.cmd -DLIB=%target-library-name(SwiftInProcPluginServer)
// RUN: %FileCheck %s --check-prefix=CMD --input-file=%t/MyApp-b.cmd -DLIB=%target-library-name(SwiftInProcPluginServer)
// CMD: -resolved-in-process-plugin-server-path
// CMD-NEXT: /^toolchain/lib/swift/host/[[LIB]]

// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend-plain -emit-module -o %t/MyApp.swiftmodule -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -module-name MyApp -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/macro.swift @%t/MyApp-a.cmd > %t/key-a.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend-plain -emit-module -o %t/MyApp.swiftmodule -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -module-name MyApp -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/macro.swift @%t/MyApp-b.cmd > %t/key-b.casid
// RUN: diff %t/key-a.casid %t/key-b.casid

/// The prefix mapped server path is mapped back to load the library plugin, and
/// it overrides '-in-process-plugin-server-path'.
// RUN: %target-swift-frontend-plain -emit-module -o %t/MyApp.swiftmodule -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -module-name MyApp -O -Rmacro-loading \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/macro.swift @%t/MyApp-b.cmd -cache-replay-prefix-map /^toolchain %t/toolchain-b \
// RUN:   -in-process-plugin-server-path %t/nonexistent/%target-library-name(SwiftInProcPluginServer) 2>&1 | %FileCheck %s --check-prefix=REMARK
// REMARK: remark: loaded macro implementation module 'MacroDefinition' from shared library

/// Without prefix mapping, the server path is still passed.
// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-serialized -module-name MyApp -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/macro.swift -o %t/deps-nomap.json -swift-version 5 -cache-compile-job -cas-path %t/cas \
// RUN:   -load-plugin-library %t/plugins/%target-library-name(MacroDefinition) \
// RUN:   -in-process-plugin-server-path %t/toolchain-a/lib/swift/host/%target-library-name(SwiftInProcPluginServer)
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps-nomap.json MyApp > %t/MyApp-nomap.cmd
// RUN: %FileCheck %s --check-prefix=CMD-NOMAP --input-file=%t/MyApp-nomap.cmd -DLIB=%target-library-name(SwiftInProcPluginServer) -DTMP=%t
// CMD-NOMAP: -resolved-in-process-plugin-server-path
// CMD-NOMAP-NEXT: [[TMP]]/toolchain-a/lib/swift/host/[[LIB]]

/// The server is not needed if the plugin is loaded by an executable.
// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-serialized -module-name MyApp -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/macro.swift -o %t/deps-exec.json -swift-version 5 -cache-compile-job -cas-path %t/cas \
// RUN:   -external-plugin-path %t/plugins#%swift-plugin-server \
// RUN:   -in-process-plugin-server-path %t/toolchain-a/lib/swift/host/%target-library-name(SwiftInProcPluginServer)
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps-exec.json MyApp > %t/MyApp-exec.cmd
// RUN: %FileCheck %s --check-prefix=CMD-EXEC --input-file=%t/MyApp-exec.cmd
// CMD-EXEC: -load-resolved-plugin
// CMD-EXEC-NOT: -resolved-in-process-plugin-server-path

//--- macro.swift
@freestanding(expression) macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")

func test(a: Int, b: Int) {
  let _ = #stringify(a + b)
}
