// RUN: %swiftc_driver -target wasm32-unknown-wasi -sdk %S/Inputs/wasm.sdk -typecheck %s
// RUN: %swiftc_driver -target wasm64-unknown-wasi -sdk %S/Inputs/wasm.sdk -typecheck %s
