// {"kind":"typecheck","original":"48b7f3ec","signature":"swift::Lexer::lexOperatorIdentifier()","useGuardMalloc":true}
// RUN: %empty-directory(%t)
// RUN: echo -n '><#' > %t/main.swift
// RUN: env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib not %target-swift-frontend -typecheck %t/main.swift
// REQUIRES: OS=macosx
// REQUIRES: no_asan
// REQUIRES: target-same-as-host
