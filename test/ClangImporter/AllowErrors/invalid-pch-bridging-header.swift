// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/pch %t/pch-dir
// RUN: split-file %s %t

// Check that the pch is output even though it has errors
// RUN: %target-swift-frontend -emit-pch -o %t/pch/bridging-header.pch -Xcc -Xclang -Xcc -fallow-pcm-with-compiler-errors -Xcc -Xclang -Xcc -fmodule-format=raw %t/bridging-header.h
// RUN: %target-swift-frontend -typecheck -verify -import-objc-header %t/pch/bridging-header.pch -Xcc -Xclang -Xcc -fallow-pcm-with-compiler-errors -Xcc -Xclang -Xcc -fmodule-format=raw %t/use.swift
// RUN: ls %t/pch/*.pch | count 1

// Same but with implicit PCH instead
// RUN: %target-swift-frontend -typecheck -verify -import-objc-header %t/bridging-header.h -pch-output-dir %t/pch-dir -Xcc -Xclang -Xcc -fallow-pcm-with-compiler-errors -Xcc -Xclang -Xcc -fmodule-format=raw %t/use.swift
// RUN: ls %t/pch-dir/*.pch | count 1

// Second implicit run since we may go down a different path if the PCH already
// exists
// RUN: %target-swift-frontend -typecheck -verify -import-objc-header %t/bridging-header.h -pch-output-dir %t/pch-dir -Xcc -Xclang -Xcc -fallow-pcm-with-compiler-errors -Xcc -Xclang -Xcc -fmodule-format=raw %t/use.swift
// RUN: ls %t/pch-dir/*.pch | count 1

//--- bridging-header.h
@import DoesNotExist;

struct SomeTy {
  int a;
};

//--- use.swift
func use(s: SomeTy) {}
