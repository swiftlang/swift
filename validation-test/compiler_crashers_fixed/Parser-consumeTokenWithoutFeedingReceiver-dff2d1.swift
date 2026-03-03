// RUN: %empty-directory(%t)

// RUN: echo    '#"'   > %t/main1.swift
// RUN: echo -n '#"'   > %t/main2.swift
// RUN: echo    '"""'  > %t/main3.swift
// RUN: echo -n '"""'  > %t/main4.swift
// RUN: echo    '#"""' > %t/main5.swift
// RUN: echo -n '#"""' > %t/main6.swift

// RUN: env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib not %target-swift-frontend -typecheck %t/main1.swift
// RUN: env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib not %target-swift-frontend -typecheck %t/main2.swift
// RUN: env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib not %target-swift-frontend -typecheck %t/main3.swift
// RUN: env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib not %target-swift-frontend -typecheck %t/main4.swift
// RUN: env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib not %target-swift-frontend -typecheck %t/main5.swift
// RUN: env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib not %target-swift-frontend -typecheck %t/main6.swift

// guardmalloc is incompatible with ASAN
// REQUIRES: no_asan
