// -- check that invalid values are flagged
// RUN: not %swiftc_driver -target x86_64-unknown-windows-msvc -libc MLd -c %s -### 2>&1 | %FileCheck %s -check-prefix CHECK-INVALID
// CHECK-INVALID: error: invalid value 'MLd' in '-libc'

// -- check that cross-compilation still succeeds
// RUN: %swiftc_driver -target x86_64-unknown-linux-gnu -c %s -###

// -- check flags for /MD
// RUN: %swiftc_driver -target x86_64-unknown-windows-msvc -libc MD -c %s -### 2>&1 | %FileCheck %s -check-prefix CHECK-MD
// CHECK-MD: -autolink-library oldnames -autolink-library msvcrt -Xcc -D_MT -Xcc -D_DLL

// -- check flags for /MDd
// RUN: %swiftc_driver -target x86_64-unknown-windows-msvc -libc MDd -c %s -### 2>&1 | %FileCheck %s -check-prefix CHECK-MDd
// CHECK-MDd: -autolink-library oldnames -autolink-library msvcrtd -Xcc -D_MT -Xcc -D_DLL

// -- check flags for /MT
// RUN: %swiftc_driver -target x86_64-unknown-windows-msvc -libc MT -c %s -### 2>&1 | %FileCheck %s -check-prefix CHECK-MT
// CHECK-MT: -autolink-library oldnames -autolink-library libcmt -Xcc -D_MT
// CHECK-MT-NOT: -D_DLL

// -- check flags for /MTd
// RUN: %swiftc_driver -target x86_64-unknown-windows-msvc -libc MTd -c %s -### 2>&1 | %FileCheck %s -check-prefix CHECK-MTd
// CHECK-MTd: -autolink-library oldnames -autolink-library libcmtd -Xcc -D_MT
// CHECK-MTd-NOT: -D_DLL

