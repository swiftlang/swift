// RUN: %empty-directory(%t)

// RUN: echo '#"' > %t/main1.swift
// RUN: echo -n '#"' > %t/main2.swift

// RUN: env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib not %target-swift-frontend -typecheck %t/main1.swift
// RUN: env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib not %target-swift-frontend -typecheck %t/main2.swift

// guardmalloc is incompatible with ASAN
// REQUIRES: no_asan
