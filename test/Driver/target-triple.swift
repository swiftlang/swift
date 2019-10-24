// RUN: %swiftc_driver -target wasm32-unknown-unknown-wasm -sdk %S/Inputs/wasm32.sdk -typecheck %s
// RUN: %swiftc_driver -target wasm64-unknown-unknown-wasm -sdk %S/Inputs/wasm64.sdk -typecheck %s
