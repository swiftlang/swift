// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)

// RUN: split-file %s %t

//#-- Prepare the Wasm macro plugin.
// RUN: /Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin/swiftc \
// RUN:   -swift-version 5 -static-stdlib -parse-as-library \
// RUN:   -o %t/Plugin.wasm \
// RUN:   -module-name MacroDefinition \
// RUN:   %t/MacroDefinition.swift \
// RUN:   %S/Inputs/wasm_plugin.swift \
// RUN:   -g -target wasm32-unknown-wasi \
// RUN:   -sdk ~/Library/org.swift.swiftpm/swift-sdks/swift-wasm-DEVELOPMENT-SNAPSHOT-2024-04-19-a-wasm32-unknown-wasi.artifactbundle/DEVELOPMENT-SNAPSHOT-2024-04-19-a-wasm32-unknown-wasi/wasm32-unknown-wasi/WASI.sdk \
// RUN:   -resource-dir ~/Library/org.swift.swiftpm/swift-sdks/swift-wasm-DEVELOPMENT-SNAPSHOT-2024-04-19-a-wasm32-unknown-wasi.artifactbundle/DEVELOPMENT-SNAPSHOT-2024-04-19-a-wasm32-unknown-wasi/wasm32-unknown-wasi/swift.xctoolchain/usr/lib/swift_static \
// RUN:   -Xcc --sysroot -Xcc ~/Library/org.swift.swiftpm/swift-sdks/swift-wasm-DEVELOPMENT-SNAPSHOT-2024-04-19-a-wasm32-unknown-wasi.artifactbundle/DEVELOPMENT-SNAPSHOT-2024-04-19-a-wasm32-unknown-wasi/wasm32-unknown-wasi/WASI.sdk \
// RUN:   -Xcc -resource-dir -Xcc ~/Library/org.swift.swiftpm/swift-sdks/swift-wasm-DEVELOPMENT-SNAPSHOT-2024-04-19-a-wasm32-unknown-wasi.artifactbundle/DEVELOPMENT-SNAPSHOT-2024-04-19-a-wasm32-unknown-wasi/wasm32-unknown-wasi/swift.xctoolchain/usr/lib/swift_static/clang \
// RUN:   -Xclang-linker -resource-dir -Xclang-linker ~/Library/org.swift.swiftpm/swift-sdks/swift-wasm-DEVELOPMENT-SNAPSHOT-2024-04-19-a-wasm32-unknown-wasi.artifactbundle/DEVELOPMENT-SNAPSHOT-2024-04-19-a-wasm32-unknown-wasi/wasm32-unknown-wasi/swift.xctoolchain/usr/lib/swift_static/clang

// RUN: env SWIFT_DUMP_PLUGIN_MESSAGING=1 %target-swift-frontend \
// RUN:   -typecheck \
// RUN:   -swift-version 5 \
// RUN:   -load-plugin %t/Plugin.wasm:%swift-plugin-server#MacroDefinition \
// RUN:   -Rmacro-loading \
// RUN:   -module-name MyApp \
// RUN:   %t/test.swift \
// RUN:   > %t/macro-loading.txt 2>&1

// RUN: %FileCheck %s < %t/macro-loading.txt

// CHECK: ->(plugin:[[#PID:]]) {"getCapability":{"capability":{"protocolVersion":[[#PROTOCOL_VERSION:]]}}}
// CHECK: <-(plugin:[[#PID]]) {"getCapabilityResult":{"capability":{"features":["load-plugin-library"],"protocolVersion":7}}}
// CHECK: ->(plugin:[[#PID]]) {"loadPluginLibrary":{"libraryPath":"{{.+}}/Plugin.wasm","moduleName":"MacroDefinition"}}
// CHECK: <-(plugin:[[#PID]]) {"loadPluginLibraryResult":{"diagnostics":[],"loaded":true}}
// CHECK: ->(plugin:[[#PID]]) {"expandFreestandingMacro":{"discriminator":"$s{{.+}}","lexicalContext":[{{.*}}],"macro":{"moduleName":"MacroDefinition","name":"constInt","typeName":"ConstMacro"},"macroRole":"expression","syntax":{"kind":"expression","location":{"column":16,"fileID":"MyApp/test.swift","fileName":"{{.+}}test.swift","line":4,"offset":143},"source":"#constInt"}}}
// CHECK: <-(plugin:[[#PID]]) {"expandMacroResult":{"diagnostics":[],"expandedSource":"1"}}

//--- test.swift
@freestanding(expression) macro constInt() -> Int = #externalMacro(module: "MacroDefinition", type: "ConstMacro")

func foo() {
  let _: Int = #constInt
}

//--- MacroDefinition.swift
let mockPlugin = #"""
[
  {
    "expect": {"expandFreestandingMacro": {
               "macro": {"moduleName": "MacroDefinition", "typeName": "ConstMacro"},
               "syntax": {"kind": "expression", "source": "#constInt"}}},
    "response": {"expandMacroResult": {"expandedSource": "1", "diagnostics": []}}
  }
]
"""#
