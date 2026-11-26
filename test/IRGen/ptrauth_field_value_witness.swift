// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift -emit-ir -Onone -target arm64e-apple-macos14.0 \
// RUN:   -import-objc-header %t/header.h %t/test.swift > %t/out.ll
// RUN: %FileCheck %t/test.swift --check-prefix=DATA < %t/out.ll
// RUN: %FileCheck %t/test.swift --check-prefix=FN < %t/out.ll
// RUN: %swift -emit-ir -O -target arm64e-apple-macos14.0 \
// RUN:   -import-objc-header %t/header.h %t/test.swift -o /dev/null

// REQUIRES: CPU=arm64e
// REQUIRES: OS=macosx

// Value witnesses must call Clang's copy helper.

//--- header.h

#include <ptrauth.h>

struct Data {
  void *_Nullable __ptrauth(ptrauth_key_process_independent_data, 1, 1234) p;
};

struct Function {
  void (*_Nullable __ptrauth(ptrauth_key_function_pointer, 1, 5678) f)(void);
};

//--- test.swift

// DATA-LABEL: define {{.*}} @"$sSo4DataVwcp"(
// DATA:   call void @__copy_assignment_{{.*}}(ptr %dest, ptr %src)
// DATA: {{^}$}}
public func makeDataArray(_ d: Data) -> [Data] {
  return [d]
}

// FN-LABEL: define {{.*}} @"$sSo8FunctionVwcp"(
// FN:   call void @__copy_assignment_{{.*}}(ptr %dest, ptr %src)
// FN: {{^}$}}
public func makeFunctionArray(_ f: Function) -> [Function] {
  return [f]
}
