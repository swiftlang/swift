// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/pch %t/pch-dir
// RUN: split-file %s %t
// RUN: sed -e "s|TEST_DIR|%/t|g" %t/hmap.json > %t/inner.json

// Check that the pch is output even though it has errors
// RUN: %target-swift-frontend -emit-pch -o %t/pch/bridging-header.pch -Xcc -Xclang -Xcc -fallow-pcm-with-compiler-errors -Xcc -Xclang -Xcc -fmodule-format=raw %t/bridging-header.h -Xcc -I%t/inner.hmap
// RUN: not %target-swift-frontend -typecheck -import-objc-header %t/pch/bridging-header.pch -Xcc -Xclang -Xcc -fallow-pcm-with-compiler-errors -Xcc -Xclang -Xcc -fmodule-format=raw %t/use.swift -Xcc -I%t/inner.hmap 2>&1 | %FileCheck %s -check-prefix MISSING_HMAP
// RUN: ls %t/pch/*.pch | count 1

// Same but with implicit PCH instead
// RUN: not %target-swift-frontend -typecheck -import-objc-header %t/bridging-header.h -pch-output-dir %t/pch-dir -Xcc -Xclang -Xcc -fallow-pcm-with-compiler-errors -Xcc -Xclang -Xcc -fmodule-format=raw %t/use.swift -Xcc -I%t/inner.hmap 2>&1 | %FileCheck %s -check-prefix MISSING_HMAP
// RUN: ls %t/pch-dir/*.pch | count 1

// Second implicit run since we may go down a different path if the PCH already
// exists
// RUN: not %target-swift-frontend -typecheck -import-objc-header %t/bridging-header.h -pch-output-dir %t/pch-dir -Xcc -Xclang -Xcc -fallow-pcm-with-compiler-errors -Xcc -Xclang -Xcc -fmodule-format=raw %t/use.swift -Xcc -I%t/inner.hmap 2>&1 | %FileCheck %s -check-prefix MISSING_HMAP
// RUN: ls %t/pch-dir/*.pch | count 1

// Create the headermap, should now have no errors
// RUN: %hmaptool write %t/inner.json %t/inner.hmap
// RUN: %target-swift-frontend -typecheck -verify -import-objc-header %t/bridging-header.h -pch-output-dir %t/pch-dir -Xcc -Xclang -Xcc -fallow-pcm-with-compiler-errors -Xcc -Xclang -Xcc -fmodule-format=raw %t/use.swift -Xcc -I%t/inner.hmap
// RUN: ls %t/pch-dir/*.pch | count 1

//--- bridging-header.h
#include "inner.h"

struct SomeTy {
  int a;
};

//--- inner/inner.h
struct InnerTy {
  int b;
};

//--- hmap.json
{
  "mappings": {
    "inner.h": "TEST_DIR/inner/inner.h"
  }
}

//--- use.swift
func use(s: SomeTy, s2: InnerTy) {}

// Note: extra newlines below ensure that context printing doesn't show the
// lines that we shouldn't see.


// MISSING_HMAP-NOT: cannot find type 'SomeTy' in scope
// MISSING_HMAP: cannot find type 'InnerTy' in scope
// MISSING_HMAP-NOT: cannot find type 'SomeTy' in scope
