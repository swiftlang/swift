// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Bridging-header PCH emitted by '-emit-pch' and consumed by '-c' must agree on
// the Clang OptimizationLevel even with '-clang-target'.

// clang-target + -O:
//
// PCH round-trip must not report "OptimizationLevel differs in precompiled file".
// RUN: %target-swift-frontend -emit-pch -o %t/O.pch %t/bridging.h -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx14.0 -O -module-cache-path %t/mcp-O
// RUN: %target-swift-frontend -c -primary-file %t/main.swift -import-objc-header %t/O.pch -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx14.0 -O -module-cache-path %t/mcp-O -module-name repro -o %t/O.o

// no clang-target + -O:
//
// RUN: %target-swift-frontend -emit-pch -o %t/plain.pch %t/bridging.h -target %target-cpu-apple-macosx13.0 -O -module-cache-path %t/mcp-plain
// RUN: %target-swift-frontend -c -primary-file %t/main.swift -import-objc-header %t/plain.pch -target %target-cpu-apple-macosx13.0 -O -module-cache-path %t/mcp-plain -module-name repro -o %t/plain.o

// clang-target, no optimization:
//
// RUN: %target-swift-frontend -emit-pch -o %t/noopt.pch %t/bridging.h -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx14.0 -module-cache-path %t/mcp-noopt
// RUN: %target-swift-frontend -c -primary-file %t/main.swift -import-objc-header %t/noopt.pch -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx14.0 -module-cache-path %t/mcp-noopt -module-name repro -o %t/noopt.o

// __OPTIMIZE__ must stay defined for an imported module across these combos.
//
// RUN: %target-swift-frontend -c -primary-file %t/mainmod.swift -I %t -module-cache-path %t/mcp-i -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx14.0 -O -module-name repro -o %t/i.o
// RUN: %target-swift-frontend -c -primary-file %t/mainmod.swift -I %t -module-cache-path %t/mcp-ii -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx14.0 -O -Xcc -O2 -module-name repro -o %t/ii.o
// RUN: %target-swift-frontend -c -primary-file %t/mainmod.swift -I %t -module-cache-path %t/mcp-iii -target %target-cpu-apple-macosx13.0 -clang-target %target-cpu-apple-macosx14.0 -Xcc -O2 -module-name repro -o %t/iii.o
// RUN: %target-swift-frontend -c -primary-file %t/mainmod.swift -I %t -module-cache-path %t/mcp-iv -target %target-cpu-apple-macosx13.0 -O -module-name repro -o %t/iv.o
// RUN: %target-swift-frontend -c -primary-file %t/mainmod.swift -I %t -module-cache-path %t/mcp-v -target %target-cpu-apple-macosx13.0 -Xcc -O2 -module-name repro -o %t/v.o

//--- bridging.h
int repro_add(int a, int b);

//--- main.swift
public func use() { _ = repro_add(1, 2) }

//--- module.modulemap
module OptGuard { header "optguard.h" export * }

//--- optguard.h
#if !defined(__OPTIMIZE__)
#error "OptGuard compiled without __OPTIMIZE__"
#endif
static inline int optguard_fn(void) { return 42; }

//--- mainmod.swift
import OptGuard
public func use() { _ = optguard_fn() }
