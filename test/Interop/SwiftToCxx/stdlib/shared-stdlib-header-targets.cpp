// The public header selects bindings without including target SDK headers.
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir -p %t/swift %t/neighbor/swift
// RUN: cp %swift_src_root/lib/PrintAsClang/Swift.h %t/swift/Swift.h
// RUN: cp %swift_src_root/lib/PrintAsClang/Swift.h %t/neighbor/swift/Swift.h
// RUN: %target-clang -E -P -x c++ -nostdinc -target arm64-apple-macosx13 %t/client.cpp -I %t | %FileCheck %s --check-prefix=MACOS
// RUN: %target-clang -E -P -x c++ -nostdinc -target arm64e-apple-macosx13 %t/client.cpp -I %t | %FileCheck %s --check-prefix=ARM64E
// RUN: %target-clang -E -P -x c++ -nostdinc -target x86_64h-apple-macosx13 %t/client.cpp -I %t | %FileCheck %s --check-prefix=X86
// RUN: mkdir -p %t/swift/macosx/x86_64h
// RUN: cp %t/haswell.h %t/swift/macosx/x86_64h/_SwiftStdlib.h
// RUN: %target-clang -E -P -x c++ -nostdinc -target x86_64h-apple-macosx13 %t/client.cpp -I %t | %FileCheck %s --check-prefix=HASWELL
// RUN: %target-clang -E -P -x c++ -nostdinc -target arm64-apple-ios15-simulator %t/client.cpp -I %t | %FileCheck %s --check-prefix=IOS-SIM
// RUN: %target-clang -E -P -x c++ -nostdinc -target arm64-apple-ios15-macabi %t/client.cpp -I %t | %FileCheck %s --check-prefix=CATALYST
// RUN: %target-clang -E -P -x c++ -nostdinc -target arm64-apple-tvos15-simulator %t/client.cpp -I %t | %FileCheck %s --check-prefix=TV-SIM
// RUN: %target-clang -E -P -x c++ -nostdinc -target arm64_32-apple-watchos8 %t/client.cpp -I %t | %FileCheck %s --check-prefix=ARM64-32
// RUN: %target-clang -E -P -x c++ -nostdinc -target arm64-apple-xros1-simulator %t/client.cpp -I %t | %FileCheck %s --check-prefix=XR-SIM
// RUN: %target-clang -E -P -x c -nostdinc -target x86_64-unknown-linux-gnu %t/client.cpp -I %t | %FileCheck %s --check-prefix=LINUX
// RUN: %target-clang -E -P -x c -nostdinc -target x86_64-unknown-linux-gnu -U__clang__ %t/client.cpp -I %t | %FileCheck %s --check-prefix=LINUX
// RUN: %target-clang -E -P -x c++ -nostdinc -target aarch64-unknown-linux-musl %t/client.cpp -I %t | %FileCheck %s --check-prefix=MUSL
// RUN: %target-clang -E -P -x c++ -nostdinc -target aarch64-swift-linux-musl %t/client.cpp -I %t | %FileCheck %s --check-prefix=LINUX-STATIC
// RUN: %target-clang -E -P -x c++ -nostdinc -target armv7-unknown-linux-androideabi %t/client.cpp -I %t | %FileCheck %s --check-prefix=ANDROID
// RUN: %target-clang -E -P -x c++ -nostdinc -target aarch64-unknown-windows-msvc %t/client.cpp -I %t | %FileCheck %s --check-prefix=WINDOWS
// RUN: %target-clang -E -P -x c++ -nostdinc -target arm64ec-unknown-windows-msvc %t/client.cpp -I %t | %FileCheck %s --check-prefix=ARM64EC
// RUN: %target-clang -E -P -x c -nostdinc -target x86_64-unknown-freebsd -U__clang__ %t/client.cpp -I %t | %FileCheck %s --check-prefix=FREEBSD
// A missing target must not find bindings in a neighboring installation.
// RUN: not %target-clang -E -P -x c++ -nostdinc -target arm64-apple-macosx13 %t/client.cpp -I %t/neighbor 2>&1 | %FileCheck %s --check-prefix=MISSING
// RUN: not %target-clang -E -P -x c++ -nostdinc -target riscv64-unknown-linux-gnu %t/client.cpp -I %t 2>&1 | %FileCheck %s --check-prefix=MISSING

// MACOS: selected_macos_arm64
// ARM64E: selected_macos_arm64e
// X86: selected_macos_x86_64
// HASWELL: selected_macos_x86_64h
// IOS-SIM: selected_ios_simulator
// CATALYST: selected_catalyst
// TV-SIM: selected_tvos_simulator
// ARM64-32: selected_watchos_arm64_32
// XR-SIM: selected_xros_simulator
// LINUX: selected_linux
// MUSL: selected_musl
// LINUX-STATIC: selected_linux_static
// ANDROID: selected_android
// WINDOWS: selected_windows_aarch64
// ARM64EC: selected_windows_arm64ec
// FREEBSD: selected_freebsd
// MISSING: Swift standard library C++ bindings are not installed for this target

//--- client.cpp
#include <swift/Swift.h>
#if defined(SWIFT_STDLIB_HEADER_PLATFORM) || defined(SWIFT_STDLIB_HEADER_ARCH)
#error "The public header must not leave target-selection macros defined"
#endif

//--- haswell.h
selected_macos_x86_64h
//--- swift/macosx/arm64/_SwiftStdlib.h
selected_macos_arm64
//--- swift/macosx/arm64e/_SwiftStdlib.h
selected_macos_arm64e
//--- swift/macosx/x86_64/_SwiftStdlib.h
selected_macos_x86_64
//--- swift/iphonesimulator/arm64/_SwiftStdlib.h
selected_ios_simulator
//--- swift/maccatalyst/arm64/_SwiftStdlib.h
selected_catalyst
//--- swift/appletvsimulator/arm64/_SwiftStdlib.h
selected_tvos_simulator
//--- swift/watchos/arm64_32/_SwiftStdlib.h
selected_watchos_arm64_32
//--- swift/xrsimulator/arm64/_SwiftStdlib.h
selected_xros_simulator
//--- swift/linux/x86_64/_SwiftStdlib.h
selected_linux
//--- swift/musl/aarch64/_SwiftStdlib.h
selected_musl
//--- swift/linux-static/aarch64/_SwiftStdlib.h
selected_linux_static
//--- swift/android/armv7/_SwiftStdlib.h
selected_android
//--- swift/windows/aarch64/_SwiftStdlib.h
selected_windows_aarch64
//--- swift/windows/arm64ec/_SwiftStdlib.h
selected_windows_arm64ec
//--- swift/freebsd/x86_64/_SwiftStdlib.h
selected_freebsd
