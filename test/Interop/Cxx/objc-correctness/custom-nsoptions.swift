// RUN: rm -rf %t
// RUN: mkdir -p %t/pch

// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -enable-objc-interop -enable-experimental-cxx-interop

// RUN: %target-swift-frontend -emit-pch -enable-objc-interop -enable-experimental-cxx-interop -o %t/pch/customNSOptions.pch %S/Inputs/customNSOptions.h
// RUN: %target-typecheck-verify-swift -D BRIDGING_HEADER -I %S/Inputs -import-objc-header %t/pch/customNSOptions.pch -enable-objc-interop -enable-experimental-cxx-interop %s

// REQUIRES: objc_interop

#if !BRIDGING_HEADER
import CustomNSOptions
#endif

let flags1: MyControlFlags = []
let flags2: MyControlFlags = [.first]
