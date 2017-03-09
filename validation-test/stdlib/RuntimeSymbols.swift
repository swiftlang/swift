# Verify symbols from libswiftCore.dylib.

# No C++ exports allowed.
RUN: %target-nm-exports %platform-module-dir/libswiftCore.dylib | %FileCheck --check-prefix CHECK-CXX %s
CHECK-CXX-NOT: __Z

# No imports of operators new and delete allowed.
# Use swift/Runtime/OperatorNew.h instead.
RUN: %target-nm-imports %platform-module-dir/libswiftCore.dylib | %FileCheck --check-prefix CHECK-NEW %s
CHECK-NEW-NOT: __Znwm
CHECK-NEW-NOT: __ZnwmRKSt9nothrow_t
CHECK-NEW-NOT: __Znam
CHECK-NEW-NOT: __ZnamRKSt9nothrow_t
CHECK-NEW-NOT: __ZdlPv
CHECK-NEW-NOT: __ZdlPvm
CHECK-NEW-NOT: __ZdlPvRKSt9nothrow_t
CHECK-NEW-NOT: __ZdaPv
CHECK-NEW-NOT: __ZdaPvm
CHECK-NEW-NOT: __ZdaPvRKSt9nothrow_t

