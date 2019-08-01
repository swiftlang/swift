// Check that there are no errors printed when trying to load an outdated PCH file
// rdar://problem/53312911

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/pch-loading-error.h %t/pch-loading-error.h
// RUN: %target-swift-frontend -parse-as-library -module-name=pch_loading_error -emit-sil %s -enable-objc-interop -import-objc-header %t/pch-loading-error.h -pch-output-dir %t/pch
// RUN: %{python} %S/../ParseableInterface/ModuleCache/Inputs/make-old.py %t/pch/*
// RUN: echo "// force newer header than pch" >> %t/pch-loading-error.h
// RUN: %target-swift-frontend -parse-as-library -module-name=pch_loading_error -emit-sil %s -enable-objc-interop -import-objc-header %t/pch-loading-error.h -pch-output-dir %t/pch 2>&1 | %FileCheck %s
// RUN: %{python} %S/../ParseableInterface/ModuleCache/Inputs/check-is-new.py %t/pch/*

// CHECK-NOT: fatal error

public func mytest(x: Int32) -> Int32 {
  return c_func(x)
}
