// REQUIRES: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -scan-dependencies -module-name NoCpp \
// RUN:   -module-cache-path %t/clang-module-cache -disable-objc-interop \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   %t/NoCpp.swift -o %t/NoCpp-deps.json -I %t 2>&1
// RUN: %target-swift-frontend -scan-dependencies -module-name Cpp \
// RUN:   -module-cache-path %t/clang-module-cache -disable-objc-interop \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   %t/Cpp.swift -o %t/Cpp-deps.json -I %t 2>&1
// RUN: %target-swift-frontend -scan-dependencies -module-name Crt \
// RUN:   -module-cache-path %t/clang-module-cache -disable-objc-interop \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   %t/Crt.swift -o %t/Crt-deps.json -I %t 2>&1
// RUN: %FileCheck %s --check-prefix=NOCPP < %t/NoCpp-deps.json
// RUN: %FileCheck %s --check-prefix=CPP < %t/Cpp-deps.json
// RUN: %FileCheck %s --check-prefix=CRT < %t/Crt-deps.json

// NOCPP:      "mainModuleName": "NoCpp"
// NOCPP:      "linkName": "swiftCxxStdlib",
// NOCPP-NEXT: "isStatic": true,

// CPP:        "mainModuleName": "Cpp"
// CPP:        "linkName": "swiftCxxStdlib",
// CPP-NEXT:   "isStatic": true,

// CRT:        "mainModuleName": "Crt"
// CRT:        "linkName": "swiftCxxStdlib",
// CRT-NEXT:   "isStatic": true,

//--- NoCpp.swift
// Empty

//--- Cpp.swift
import CxxStdlib

//--- Crt.swift
import CRT



