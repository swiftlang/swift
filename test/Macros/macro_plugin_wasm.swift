// REQUIRES: swift_swift_parser
// REQUIRES: CODEGENERATOR=WebAssembly

// RUN: %empty-directory(%t)

// RUN: split-file %s %t

//#-- Prepare the Wasm macro plugin.
// RUN: %swift-build-wasm-c-plugin %t/MacroDefinition.c -o %t/Plugin.wasm

// RUN: env SWIFT_DUMP_PLUGIN_MESSAGING=1 %target-swift-frontend \
// RUN:   -typecheck \
// RUN:   -swift-version 5 \
// RUN:   -load-resolved-plugin %t/Plugin.wasm#%swift-plugin-server#MacroDefinition \
// RUN:   -Rmacro-loading \
// RUN:   -module-name MyApp \
// RUN:   %t/test.swift \
// RUN:   > %t/macro-loading.txt 2>&1

// RUN: %FileCheck %s < %t/macro-loading.txt

// CHECK: ->(plugin:[[#PID:]]) {"getCapability":{"capability":{"protocolVersion":[[#PROTOCOL_VERSION:]]}}}
// CHECK: <-(plugin:[[#PID]]) {"getCapabilityResult":{"capability":{"features":["load-plugin-library"],"protocolVersion":[[#PROTOCOL_VERSION]]}}}
// CHECK: ->(plugin:[[#PID]]) {"loadPluginLibrary":{"libraryPath":"{{.*[\\/]}}Plugin.wasm","moduleName":"MacroDefinition"}}
// CHECK: <-(plugin:[[#PID]]) {"loadPluginLibraryResult":{"diagnostics":[],"loaded":true}}
// CHECK: ->(plugin:[[#PID]]) {"expandFreestandingMacro":{"discriminator":"$s{{.+}}","lexicalContext":[{{.*}}],"macro":{"moduleName":"MacroDefinition","name":"constInt","typeName":"ConstMacro"},"macroRole":"expression","staticBuildConfiguration":{{.*}},"syntax":{"kind":"expression","location":{"column":16,"fileID":"MyApp/test.swift","fileName":"{{.+}}test.swift","line":4,"offset":143},"source":"#constInt"}}}
// CHECK: <-(plugin:[[#PID]]) {"expandMacroResult":{"diagnostics":[],"expandedSource":"1"}}

//--- test.swift
@freestanding(expression) macro constInt() -> Int = #externalMacro(module: "MacroDefinition", type: "ConstMacro")

func foo() {
  let _: Int = #constInt
}

//--- MacroDefinition.c
#include "Inputs/wasi_shim.h"

static void write_json(const char *json) {
  wasi_size_t len = swift_strlen(json);
  uint64_t len64 = (uint64_t)len;
  swift_write(1, &len64, sizeof(len64));
  swift_write(1, json, len);
}

// Read exactly 'len' bytes (the host frames each message as length + body).
static void read_fully(int fd, void *buf, wasi_size_t len) {
  uint8_t *p = (uint8_t *)buf;
  wasi_size_t total = 0;
  while (total < len) {
    wasi_size_t n = swift_read(fd, p + total, len - total);
    if (n == 0) break; // EOF
    total += n;
  }
}

// Returns 1 if 'needle' occurs within the first 'len' bytes of 'buf'.
static int contains(const char *buf, wasi_size_t len, const char *needle) {
  wasi_size_t nlen = swift_strlen(needle);
  if (nlen == 0 || nlen > len) return 0;
  for (wasi_size_t i = 0; i + nlen <= len; i++) {
    wasi_size_t j = 0;
    while (j < nlen && buf[i + j] == needle[j]) j++;
    if (j == nlen) return 1;
  }
  return 0;
}

int was_start_called = 0;

__attribute__((export_name("_start")))
void _start(void) {
  if (was_start_called) swift_abort("_start called twice!");
  was_start_called = 1;
}

__attribute__((export_name("swift_wasm_macro_v1_pump")))
void pump(void) {
  if (!was_start_called) swift_abort("_start not called!");

  // Each pump handles one framed request: 8-byte little-endian length, then body.
  uint64_t len64 = 0;
  read_fully(0, &len64, sizeof(len64));
  wasi_size_t len = (wasi_size_t)len64;
  static char request[4096];
  if (len > sizeof(request)) len = sizeof(request);
  read_fully(0, request, len);

  // The server forwards 'getCapability' before any expansion; answer it, else produce
  // the canned expansion result. Substring routing suffices for this fixed test input
  // (the expand request never contains the bytes "getCapability").
  if (contains(request, len, "getCapability")) {
    write_json("{\"getCapabilityResult\": {\"capability\": {\"protocolVersion\": 8, \"features\": [\"load-plugin-library\"]}}}");
  } else {
    write_json("{\"expandMacroResult\": {\"expandedSource\": \"1\", \"diagnostics\": []}}");
  }
}
