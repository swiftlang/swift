// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=windows-msvc

// WinRT remains part of the API surface imported through WinSDK even though
// its Clang submodule is defined by an external component module map.

import WinSDK

let _: RO_INIT_TYPE = RO_INIT_MULTITHREADED
let _ = RoInitialize
