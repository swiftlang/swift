// Must be able to run xcrun-return-self.sh
// REQUIRES: shell
// REQUIRES: rdar65281056
// FIXME: When this is turned on, please move the test from linker-library-with-space.swift
// to this file and remove that file.
// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-macosx10.9 %s 2>&1 > %t.simple.txt
// RUN: %FileCheck %s < %t.simple.txt
// RUN: %FileCheck -check-prefix SIMPLE %s < %t.simple.txt

// RUN: not %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-macosx10.9 -static-stdlib %s 2>&1 | %FileCheck -check-prefix=SIMPLE_STATIC %s

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-ios7.1-simulator %s 2>&1 > %t.simple.txt
// RUN: %FileCheck -check-prefix IOS_SIMPLE %s < %t.simple.txt

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-tvos9.0-simulator %s 2>&1 > %t.simple.txt
// RUN: %FileCheck -check-prefix tvOS_SIMPLE %s < %t.simple.txt

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target i386-apple-watchos2.0-simulator %s 2>&1 > %t.simple.txt
// RUN: %FileCheck -check-prefix watchOS_SIMPLE %s < %t.simple.txt

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-unknown-linux-gnu -Ffoo -Fsystem car -F cdr -framework bar -Lbaz -lboo -Xlinker -undefined %s 2>&1 > %t.linux.txt
// RUN: %FileCheck -check-prefix LINUX-x86_64 %s < %t.linux.txt

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target armv6-unknown-linux-gnueabihf -Ffoo -Fsystem car -F cdr -framework bar -Lbaz -lboo -Xlinker -undefined %s 2>&1 > %t.linux.txt
// RUN: %FileCheck -check-prefix LINUX-armv6 %s < %t.linux.txt

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target armv7-unknown-linux-gnueabihf -Ffoo -Fsystem car -F cdr -framework bar -Lbaz -lboo -Xlinker -undefined %s 2>&1 > %t.linux.txt
// RUN: %FileCheck -check-prefix LINUX-armv7 %s < %t.linux.txt

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target thumbv7-unknown-linux-gnueabihf -Ffoo -Fsystem car -F cdr -framework bar -Lbaz -lboo -Xlinker -undefined %s 2>&1 > %t.linux.txt
// RUN: %FileCheck -check-prefix LINUX-thumbv7 %s < %t.linux.txt

// RUN: %swiftc_driver_plain -driver-print-jobs -target armv7-unknown-linux-androideabi -Ffoo -Fsystem car -F cdr -framework bar -Lbaz -lboo -Xlinker -undefined %s 2>&1 > %t.android.txt
// RUN: %FileCheck -check-prefix ANDROID-armv7 %s < %t.android.txt
// RUN: %FileCheck -check-prefix ANDROID-armv7-NEGATIVE %s < %t.android.txt

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-unknown-windows-cygnus -Ffoo -Fsystem car -F cdr -framework bar -Lbaz -lboo -Xlinker -undefined %s 2>&1 > %t.cygwin.txt
// RUN: %FileCheck -check-prefix CYGWIN-x86_64 %s < %t.cygwin.txt

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-unknown-windows-msvc -Ffoo -Fsystem car -F cdr -framework bar -Lbaz -lboo -Xlinker -undefined %s 2>&1 > %t.windows.txt
// RUN: %FileCheck -check-prefix WINDOWS-x86_64 %s < %t.windows.txt

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target amd64-unknown-openbsd -Ffoo -Fsystem car -F cdr -framework bar -Lbaz -lboo -Xlinker -undefined %s 2>&1 > %t.openbsd.txt
// RUN: %FileCheck -check-prefix OPENBSD-amd64 %s < %t.openbsd.txt

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -emit-library -target x86_64-unknown-linux-gnu %s -Lbar -o dynlib.out 2>&1 > %t.linux.dynlib.txt
// RUN: %FileCheck -check-prefix LINUX_DYNLIB-x86_64 %s < %t.linux.dynlib.txt

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -emit-library -target x86_64-apple-macosx10.9.1 %s -sdk %S/../Inputs/clang-importer-sdk -lfoo -framework bar -Lbaz -Fgarply -Fsystem car -F cdr -Xlinker -undefined -Xlinker dynamic_lookup -o sdk.out 2>&1 > %t.complex.txt
// RUN: %FileCheck %s < %t.complex.txt
// RUN: %FileCheck -check-prefix COMPLEX %s < %t.complex.txt

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-ios7.1-simulator -Xlinker -rpath -Xlinker customrpath -L foo %s 2>&1 > %t.simple.txt
// RUN: %FileCheck -check-prefix IOS-linker-order %s < %t.simple.txt

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target armv7-unknown-linux-gnueabihf -Xlinker -rpath -Xlinker customrpath -L foo %s 2>&1 > %t.linux.txt
// RUN: %FileCheck -check-prefix LINUX-linker-order %s < %t.linux.txt

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-unknown-linux-gnu -Xclang-linker -foo -Xclang-linker foopath %s 2>&1 > %t.linux.txt
// RUN: %FileCheck -check-prefix LINUX-clang-linker-order %s < %t.linux.txt

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-unknown-windows-msvc -Xclang-linker -foo -Xclang-linker foopath %s 2>&1 > %t.windows.txt
// RUN: %FileCheck -check-prefix WINDOWS-clang-linker-order %s < %t.windows.txt

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target wasm32-unknown-wasi -Xclang-linker -flag -Xclang-linker arg %s 2>&1 | %FileCheck -check-prefix WASI-clang-linker-order %s

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-macosx10.9 -g %s | %FileCheck -check-prefix DEBUG %s

// RUN: %swiftc_driver_plain -driver-print-jobs -target x86_64-unknown-linux-gnu -toolchain-stdlib-rpath %s 2>&1 | %FileCheck -check-prefix LINUX-STDLIB-RPATH %s
// RUN: %swiftc_driver_plain -driver-print-jobs -target x86_64-unknown-linux-gnu -no-toolchain-stdlib-rpath %s 2>&1 | %FileCheck -check-prefix LINUX-NO-STDLIB-RPATH %s

// RUN: %swiftc_driver_plain -driver-print-jobs -target armv7-unknown-linux-androideabi -toolchain-stdlib-rpath %s 2>&1 | %FileCheck -check-prefix ANDROID-STDLIB-RPATH %s
// RUN: %swiftc_driver_plain -driver-print-jobs -target armv7-unknown-linux-androideabi -no-toolchain-stdlib-rpath %s 2>&1 | %FileCheck -check-prefix ANDROID-NO-STDLIB-RPATH %s

// RUN: %empty-directory(%t)
// RUN: touch %t/a.o
// RUN: touch %t/a.swiftmodule
// RUN: touch %t/b.o
// RUN: touch %t/b.swiftmodule
// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-macosx10.9 %s %t/a.o %t/a.swiftmodule %t/b.o %t/b.swiftmodule -o linker | %FileCheck -check-prefix LINK-SWIFTMODULES %s

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-macosx10.10   %s > %t.simple-macosx10.10.txt
// RUN: %FileCheck %s < %t.simple-macosx10.10.txt
// RUN: %FileCheck -check-prefix SIMPLE %s < %t.simple-macosx10.10.txt

// RUN: %empty-directory(%t)
// RUN: echo "int dummy;" >%t/a.cpp
// RUN: cc -c %t/a.cpp -o %t/a.o
// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-macosx10.9 %s %t/a.o -o linker 2>&1 | %FileCheck -check-prefix COMPILE_AND_LINK %s
// RUN: %swiftc_driver -sdk "" -save-temps -driver-print-jobs  -target x86_64-apple-macosx10.9 %s %t/a.o -driver-filelist-threshold=0 -o linker  2>&1 | tee %t/forFilelistCapture | %FileCheck -check-prefix FILELIST %s

// Extract filelist name and check it out
// RUN: tail -1 %t/forFilelistCapture | sed 's/.*-filelist //' | sed 's/ .*//' >%t/filelistName
// RUN: %FileCheck -check-prefix FILELIST-CONTENTS %s < `cat %t/filelistName`

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-macosx10.9 -emit-library %s -module-name LINKER | %FileCheck -check-prefix INFERRED_NAME_DARWIN %s
// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-unknown-linux-gnu -emit-library %s -module-name LINKER | %FileCheck -check-prefix INFERRED_NAME_LINUX %s
// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-unknown-windows-cygnus -emit-library %s -module-name LINKER | %FileCheck -check-prefix INFERRED_NAME_WINDOWS %s
// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-unknown-windows-msvc -emit-library %s -module-name LINKER | %FileCheck -check-prefix INFERRED_NAME_WINDOWS %s
// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target wasm32-unknown-wasi -emit-library %s -module-name LINKER | %FileCheck -check-prefix INFERRED_NAME_WASI %s

// Here we specify an output file name using '-o'. For ease of writing these
// tests, we happen to specify the same file name as is inferred in the
// INFERRED_NAMED_DARWIN tests above: 'libLINKER.dylib'.
// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-macosx10.9 -emit-library %s -o libLINKER.dylib | %FileCheck -check-prefix INFERRED_NAME_DARWIN %s

// On Darwin, when C++ interop is turned on, we link against libc++ explicitly.
// So also run a test where C++ interop is turned off to make sure we don't link
// against libc++ in this case.
// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-ios7.1 %s 2>&1 | %FileCheck -check-prefix IOS-no-cxx-interop %s
// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-ios7.1 -enable-experimental-cxx-interop %s 2>&1 | %FileCheck -check-prefix IOS-cxx-interop-libcxx %s

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-unknown-linux-gnu -enable-experimental-cxx-interop %s 2>&1 | %FileCheck -check-prefix LINUX-cxx-interop %s

// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-unknown-windows-msvc -enable-experimental-cxx-interop %s 2>&1 | %FileCheck -check-prefix WINDOWS-cxx-interop %s

// Check reading the SDKSettings.json from an SDK
// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-macosx10.9 -sdk %S/Inputs/MacOSX10.15.versioned.sdk %s 2>&1 | %FileCheck -check-prefix MACOS_10_15 %s
// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-macosx10.9 -sdk %S/Inputs/MacOSX10.15.4.versioned.sdk %s 2>&1 | %FileCheck -check-prefix MACOS_10_15_4 %s
// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-macosx10.9 -sdk %S/Inputs/MacOSX10.15.sdk %s 2>&1 | %FileCheck -check-prefix MACOS_UNVERSIONED %s

// Check arm64 macOS first deployment version adjustment.
// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target arm64-apple-macosx10.15.1 %s 2>&1 | %FileCheck -check-prefix MACOS_11_0 %s

// Check x86 macOS 11 deployment version adjustment is gone.
// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target x86_64-apple-macosx11.0 %s 2>&1 | %FileCheck -check-prefix MACOS_11_0 %s
// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target arm64-apple-macosx11.0 %s 2>&1 | %FileCheck -check-prefix MACOS_11_0 %s

// Check arm64 simulators first deployment version adjustment.
// RUN: %swiftc_driver -sdk "" -driver-print-jobs -target arm64-apple-ios13.0-simulator %s 2>&1 | %FileCheck -check-prefix ARM64_IOS_SIMULATOR_LINKER %s


// MACOS_10_15: -platform_version macos 10.9.0 10.15.0
// MACOS_10_15_4: -platform_version macos 10.9.0 10.15.4
// MACOS_11_0: -platform_version macos 11.0.0
// MACOS_UNVERSIONED: -platform_version macos 10.9.0 0.0.0

// X86_64_WATCHOS_SIM_LINKER: -platform_version watchos-simulator 7.0.0
// ARM64_IOS_SIMULATOR_LINKER: -platform_version ios-simulator 14.0.0

// There are more RUN lines further down in the file.

// CHECK: swift
// CHECK: -o [[OBJECTFILE:.*]]

// CHECK-NEXT: {{(bin/)?}}ld{{"? }}
// CHECK-DAG: [[OBJECTFILE]]
// CHECK-DAG: -L [[STDLIB_PATH:[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)macosx]]
// CHECK-DAG: -rpath [[STDLIB_PATH]]
// CHECK-DAG: -lSystem
// CHECK-DAG: -arch x86_64
// CHECK: -o {{[^ ]+}}


// SIMPLE: {{(bin/)?}}ld{{"? }}
// SIMPLE-NOT: -syslibroot
// SIMPLE: -platform_version macos 10.{{[0-9]+}}.{{[0-9]+}} 0.0.0
// SIMPLE-NOT: -syslibroot
// SIMPLE: -o linker


// SIMPLE_STATIC: error: -static-stdlib is no longer supported on Apple platforms


// IOS_SIMPLE: swift
// IOS_SIMPLE: -o [[OBJECTFILE:.*]]

// IOS_SIMPLE: {{(bin/)?}}ld{{"? }}
// IOS_SIMPLE-DAG: [[OBJECTFILE]]
// IOS_SIMPLE-DAG: -L {{[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)iphonesimulator}}
// IOS_SIMPLE-DAG: -lSystem
// IOS_SIMPLE-DAG: -arch x86_64
// IOS_SIMPLE-DAG: -platform_version ios-simulator 7.1.{{[0-9]+}} 0.0.0
// IOS_SIMPLE: -o linker


// tvOS_SIMPLE: swift
// tvOS_SIMPLE: -o [[OBJECTFILE:.*]]

// tvOS_SIMPLE: {{(bin/)?}}ld{{"? }}
// tvOS_SIMPLE-DAG: [[OBJECTFILE]]
// tvOS_SIMPLE-DAG: -L {{[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)appletvsimulator}}
// tvOS_SIMPLE-DAG: -lSystem
// tvOS_SIMPLE-DAG: -arch x86_64
// tvOS_SIMPLE-DAG: -platform_version tvos-simulator 9.0.{{[0-9]+}} 0.0.0
// tvOS_SIMPLE: -o linker


// watchOS_SIMPLE: swift
// watchOS_SIMPLE: -o [[OBJECTFILE:.*]]

// watchOS_SIMPLE: {{(bin/)?}}ld{{"? }}
// watchOS_SIMPLE-DAG: [[OBJECTFILE]]
// watchOS_SIMPLE-DAG: -L {{[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)watchsimulator}}
// watchOS_SIMPLE-DAG: -lSystem
// watchOS_SIMPLE-DAG: -arch i386
// watchOS_SIMPLE-DAG: -platform_version watchos-simulator 2.0.{{[0-9]+}} 0.0.0
// watchOS_SIMPLE: -o linker


// LINUX-x86_64: swift
// LINUX-x86_64: -o [[OBJECTFILE:.*]]

// LINUX-x86_64: clang{{(\.exe)?"? }}
// LINUX-x86_64-DAG: -pie
// LINUX-x86_64-DAG: [[OBJECTFILE]]
// LINUX-x86_64-DAG: -lswiftCore
// LINUX-x86_64-DAG: -L [[STDLIB_PATH:[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)]]
// LINUX-x86_64-DAG: -Xlinker -rpath -Xlinker [[STDLIB_PATH]]
// LINUX-x86_64-DAG: -F foo -iframework car -F cdr
// LINUX-x86_64-DAG: -framework bar
// LINUX-x86_64-DAG: -L baz
// LINUX-x86_64-DAG: -lboo
// LINUX-x86_64-DAG: -Xlinker -undefined
// LINUX-x86_64: -o linker

// LINUX-armv6: swift
// LINUX-armv6: -o [[OBJECTFILE:.*]]

// LINUX-armv6: clang{{(\.exe)?"? }}
// LINUX-armv6-DAG: -pie
// LINUX-armv6-DAG: [[OBJECTFILE]]
// LINUX-armv6-DAG: -lswiftCore
// LINUX-armv6-DAG: -L [[STDLIB_PATH:[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)]]
// LINUX-armv6-DAG: -target armv6-unknown-linux-gnueabihf
// LINUX-armv6-DAG: -Xlinker -rpath -Xlinker [[STDLIB_PATH]]
// LINUX-armv6-DAG: -F foo -iframework car -F cdr
// LINUX-armv6-DAG: -framework bar
// LINUX-armv6-DAG: -L baz
// LINUX-armv6-DAG: -lboo
// LINUX-armv6-DAG: -Xlinker -undefined
// LINUX-armv6: -o linker

// LINUX-armv7: swift
// LINUX-armv7: -o [[OBJECTFILE:.*]]

// LINUX-armv7: clang{{(\.exe)?"? }}
// LINUX-armv7-DAG: -pie
// LINUX-armv7-DAG: [[OBJECTFILE]]
// LINUX-armv7-DAG: -lswiftCore
// LINUX-armv7-DAG: -L [[STDLIB_PATH:[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)]]
// LINUX-armv7-DAG: -target armv7-unknown-linux-gnueabihf
// LINUX-armv7-DAG: -Xlinker -rpath -Xlinker [[STDLIB_PATH]]
// LINUX-armv7-DAG: -F foo -iframework car -F cdr
// LINUX-armv7-DAG: -framework bar
// LINUX-armv7-DAG: -L baz
// LINUX-armv7-DAG: -lboo
// LINUX-armv7-DAG: -Xlinker -undefined
// LINUX-armv7: -o linker

// LINUX-thumbv7: swift
// LINUX-thumbv7: -o [[OBJECTFILE:.*]]

// LINUX-thumbv7: clang{{(\.exe)?"? }}
// LINUX-thumbv7-DAG: -pie
// LINUX-thumbv7-DAG: [[OBJECTFILE]]
// LINUX-thumbv7-DAG: -lswiftCore
// LINUX-thumbv7-DAG: -L [[STDLIB_PATH:[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)]]
// LINUX-thumbv7-DAG: -target thumbv7-unknown-linux-gnueabihf
// LINUX-thumbv7-DAG: -Xlinker -rpath -Xlinker [[STDLIB_PATH]]
// LINUX-thumbv7-DAG: -F foo -iframework car -F cdr
// LINUX-thumbv7-DAG: -framework bar
// LINUX-thumbv7-DAG: -L baz
// LINUX-thumbv7-DAG: -lboo
// LINUX-thumbv7-DAG: -Xlinker -undefined
// LINUX-thumbv7: -o linker

// ANDROID-armv7: swift
// ANDROID-armv7: -o [[OBJECTFILE:.*]]

// ANDROID-armv7: clang{{(\.exe)?"? }}
// ANDROID-armv7-DAG: -pie
// ANDROID-armv7-DAG: [[OBJECTFILE]]
// ANDROID-armv7-DAG: -lswiftCore
// ANDROID-armv7-DAG: -L [[STDLIB_PATH:[^ ]+(/|\\\\)lib(/|\\\\)swift]]
// ANDROID-armv7-DAG: -target armv7-unknown-linux-androideabi
// ANDROID-armv7-DAG: -F foo -iframework car -F cdr
// ANDROID-armv7-DAG: -framework bar
// ANDROID-armv7-DAG: -L baz
// ANDROID-armv7-DAG: -lboo
// ANDROID-armv7-DAG: -Xlinker -undefined
// ANDROID-armv7: -o linker
// ANDROID-armv7-NEGATIVE-NOT: -Xlinker -rpath

// CYGWIN-x86_64: swift
// CYGWIN-x86_64: -o [[OBJECTFILE:.*]]

// CYGWIN-x86_64: clang{{(\.exe)?"? }}
// CYGWIN-x86_64-DAG: [[OBJECTFILE]]
// CYGWIN-x86_64-DAG: -lswiftCore
// CYGWIN-x86_64-DAG: -L [[STDLIB_PATH:[^ ]+(/|\\\\)lib(/|\\\\)swift]]
// CYGWIN-x86_64-DAG: -Xlinker -rpath -Xlinker [[STDLIB_PATH]]
// CYGWIN-x86_64-DAG: -F foo -iframework car -F cdr
// CYGWIN-x86_64-DAG: -framework bar
// CYGWIN-x86_64-DAG: -L baz
// CYGWIN-x86_64-DAG: -lboo
// CYGWIN-x86_64-DAG: -Xlinker -undefined
// CYGWIN-x86_64: -o linker

// WINDOWS-x86_64: swift
// WINDOWS-x86_64: -o [[OBJECTFILE:.*]]

// WINDOWS-x86_64: clang{{(\.exe)?"? }}
// WINDOWS-x86_64-DAG: [[OBJECTFILE]]
// WINDOWS-x86_64-DAG: -L [[STDLIB_PATH:[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)windows(/|\\\\)x86_64]]
// WINDOWS-x86_64-DAG: -F foo -iframework car -F cdr
// WINDOWS-x86_64-DAG: -framework bar
// WINDOWS-x86_64-DAG: -L baz
// WINDOWS-x86_64-DAG: -lboo
// WINDOWS-x86_64-DAG: -Xlinker -undefined
// WINDOWS-x86_64: -o linker

// OPENBSD-amd64: swift
// OPENBSD-amd64: -o [[OBJECTFILE:.*]]

// OPENBSD-amd64: clang
// OPENBSD-amd64-DAG: -fuse-ld=lld
// OPENBSD-amd64-DAG: [[OBJECTFILE]]
// OPENBSD-amd64-DAG: -lswiftCore
// OPENBSD-amd64-DAG: -L [[STDLIB_PATH:[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)]]
// OPENBSD-amd64-DAG: -Xlinker -rpath -Xlinker [[STDLIB_PATH]]
// OPENBSD-amd64-DAG: -F foo -iframework car -F cdr
// OPENBSD-amd64-DAG: -framework bar
// OPENBSD-amd64-DAG: -L baz
// OPENBSD-amd64-DAG: -lboo
// OPENBSD-amd64-DAG: -Xlinker -undefined
// OPENBSD-amd64: -o linker


// COMPLEX: {{(bin/)?}}ld{{"? }}
// COMPLEX-DAG: -dylib
// COMPLEX-DAG: -syslibroot {{.*}}/Inputs/clang-importer-sdk
// COMPLEX-DAG: -lfoo
// COMPLEX-DAG: -framework bar
// COMPLEX-DAG: -L baz
// COMPLEX-DAG: -F garply -F car -F cdr
// COMPLEX-DAG: -undefined dynamic_lookup
// COMPLEX-DAG: -platform_version macos 10.9.1 0.0.0
// COMPLEX: -o sdk.out

// LINUX_DYNLIB-x86_64: swift
// LINUX_DYNLIB-x86_64: -o [[OBJECTFILE:.*]]
// LINUX_DYNLIB-x86_64: -o {{"?}}[[AUTOLINKFILE:.*]]

// LINUX_DYNLIB-x86_64: clang{{(\.exe)?"? }}
// LINUX_DYNLIB-x86_64-DAG: -shared
// LINUX_DYNLIB-x86_64-NOT: -pie
// LINUX_DYNLIB-x86_64-DAG: -Xlinker -rpath -Xlinker [[STDLIB_PATH:[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)linux]]
// LINUX_DYNLIB-x86_64: [[STDLIB_PATH]]{{/|\\\\}}x86_64{{/|\\\\}}swiftrt.o
// LINUX_DYNLIB-x86_64-DAG: [[OBJECTFILE]]
// LINUX_DYNLIB-x86_64-DAG: @[[AUTOLINKFILE]]
// LINUX_DYNLIB-x86_64-DAG: [[STDLIB_PATH]]
// LINUX_DYNLIB-x86_64-DAG: -lswiftCore
// LINUX_DYNLIB-x86_64-DAG: -L bar
// LINUX_DYNLIB-x86_64: -o dynlib.out

// IOS-linker-order: swift
// IOS-linker-order: -o [[OBJECTFILE:.*]]

// IOS-linker-order: {{(bin/)?}}ld{{"? }}
// IOS-linker-order: -rpath [[STDLIB_PATH:[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)iphonesimulator]]
// IOS-linker-order: -L foo
// IOS-linker-order: -rpath customrpath
// IOS-linker-order: -o {{.*}}

// LINUX-linker-order: swift
// LINUX-linker-order: -o [[OBJECTFILE:.*]]

// LINUX-linker-order: clang{{(\.exe)?"? }}
// LINUX-linker-order: -Xlinker -rpath -Xlinker {{[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)linux}}
// LINUX-linker-order: -L foo
// LINUX-linker-order: -Xlinker -rpath -Xlinker customrpath
// LINUX-linker-order: -o {{.*}}

// LINUX-clang-linker-order: swift
// LINUX-clang-linker-order: -o [[OBJECTFILE:.*]]

// LINUX-clang-linker-order: clang{{"? }}
// LINUX-clang-linker-order: -foo foopath
// LINUX-clang-linker-order: -o {{.*}}

// WINDOWS-clang-linker-order: swift
// WINDOWS-clang-linker-order: -o [[OBJECTFILE:.*]]

// WINDOWS-clang-linker-order: clang{{"? }}
// WINDOWS-clang-linker-order: -foo foopath
// WINDOWS-clang-linker-order: -o {{.*}}

// WASI-clang-linker-order: swift
// WASI-clang-linker-order: -o [[OBJECTFILE:.*]]

// WASI-clang-linker-order: clang{{"? }}
// WASI-clang-linker-order: -flag arg
// WASI-clang-linker-order: -o {{.*}}

// DEBUG: bin{{/|\\\\}}swift{{c?(\.EXE)?}}
// DEBUG-NEXT: bin{{/|\\\\}}swift{{c?(\.EXE)?}}
// DEBUG-NEXT: {{(bin/)?}}ld{{"? }}
// DEBUG: -add_ast_path {{.*(/|\\\\)[^/]+}}.swiftmodule
// DEBUG: -o linker
// DEBUG-NEXT: bin{{/|\\\\}}dsymutil
// DEBUG: linker
// DEBUG: -o linker.dSYM

// LINK-SWIFTMODULES: bin{{/|\\\\}}swift{{c?(\.EXE)?}}
// LINK-SWIFTMODULES-NEXT: {{(bin/)?}}ld{{"? }}
// LINK-SWIFTMODULES-SAME: -add_ast_path {{.*}}/a.swiftmodule
// LINK-SWIFTMODULES-SAME: -add_ast_path {{.*}}/b.swiftmodule
// LINK-SWIFTMODULES-SAME: -o linker

// COMPILE_AND_LINK: bin{{/|\\\\}}swift{{c?(\.EXE)?}}
// COMPILE_AND_LINK-NOT: /a.o
// COMPILE_AND_LINK: linker.swift
// COMPILE_AND_LINK-NOT: /a.o
// COMPILE_AND_LINK-NEXT: {{(bin/)?}}ld{{"? }}
// COMPILE_AND_LINK-DAG: /a.o
// COMPILE_AND_LINK-DAG: .o
// COMPILE_AND_LINK: -o linker


// FILELIST: {{(bin/)?}}ld{{"? }}
// FILELIST-NOT: .o{{"? }}
// FILELIST: -filelist {{"?[^-]}}
// FILELIST-NOT: .o{{"? }}
// FILELIST: -o linker

// FILELIST-CONTENTS: /linker-{{.*}}.o
// FILELIST-CONTENTS: /a.o

// INFERRED_NAME_DARWIN: bin{{/|\\\\}}swift{{c?(\.EXE)?}}
// INFERRED_NAME_DARWIN: -module-name LINKER
// INFERRED_NAME_DARWIN: {{(bin/)?}}ld{{"? }}
// INFERRED_NAME_DARWIN:  -o libLINKER.dylib
// INFERRED_NAME_LINUX:   -o libLINKER.so
// INFERRED_NAME_WINDOWS: -o LINKER.dll
// INFERRED_NAME_WASI: -o libLINKER.so

// Instead of a single "NOT" check for this run, we would really want to check
// for all of the driver arguments that we _do_ expect, and then use an
// --implicit-check-not to check that -lc++ doesn't occur.
// However, --implicit-check-not has a bug where it fails to flag the
// unexpected text when it occurs after text matched by a CHECK-DAG; see
// https://bugs.llvm.org/show_bug.cgi?id=45629
// For this reason, we use a single "NOT" check for the time being here.
// The same consideration applies to the Linux and Windows cases below.
// IOS-no-cxx-interop-NOT: -lc++

// IOS-cxx-interop-libcxx: swift
// IOS-cxx-interop-libcxx-DAG: -enable-experimental-cxx-interop
// IOS-cxx-interop-libcxx-DAG: -o [[OBJECTFILE:.*]]

// IOS-cxx-interop-libcxx: {{(bin/)?}}ld{{"? }}
// IOS-cxx-interop-libcxx-DAG: [[OBJECTFILE]]
// IOS-cxx-interop-libcxx-DAG: -lc++
// IOS-cxx-interop-libcxx: -o linker

// LINUX-cxx-interop-NOT: -stdlib

// WINDOWS-cxx-interop-NOT: -stdlib

// Test ld detection. We use hard links to make sure
// the Swift driver really thinks it's been moved.

// RUN: rm -rf %t
// RUN: %empty-directory(%t/DISTINCTIVE-PATH/usr/bin)
// RUN: touch %t/DISTINCTIVE-PATH/usr/bin/ld
// RUN: chmod +x %t/DISTINCTIVE-PATH/usr/bin/ld
// RUN: %hardlink-or-copy(from: %swift_frontend_plain, to: %t/DISTINCTIVE-PATH/usr/bin/swiftc)
// RUN: %t/DISTINCTIVE-PATH/usr/bin/swiftc -target x86_64-apple-macosx10.9 %s -### | %FileCheck -check-prefix=RELATIVE-LINKER %s

// RELATIVE-LINKER: {{/|\\\\}}DISTINCTIVE-PATH{{/|\\\\}}usr{{/|\\\\}}bin{{/|\\\\}}swift
// RELATIVE-LINKER: {{/|\\\\}}DISTINCTIVE-PATH{{/|\\\\}}usr{{/|\\\\}}bin{{/|\\\\}}ld
// RELATIVE-LINKER: -o {{[^ ]+}}

// Also test arclite detection. This uses xcrun to find arclite when it's not
// next to Swift.

// RUN: %empty-directory(%t/ANOTHER-DISTINCTIVE-PATH/usr/bin)
// RUN: %empty-directory(%t/ANOTHER-DISTINCTIVE-PATH/usr/lib/arc)
// RUN: cp %S/Inputs/xcrun-return-self.sh %t/ANOTHER-DISTINCTIVE-PATH/usr/bin/xcrun

// RUN: env PATH=%t/ANOTHER-DISTINCTIVE-PATH/usr/bin %t/DISTINCTIVE-PATH/usr/bin/swiftc -target x86_64-apple-macosx10.9 %s -### | %FileCheck -check-prefix=XCRUN_ARCLITE %s

// XCRUN_ARCLITE: bin{{/|\\\\}}ld
// XCRUN_ARCLITE: {{/|\\\\}}ANOTHER-DISTINCTIVE-PATH{{/|\\\\}}usr{{/|\\\\}}lib{{/|\\\\}}arc{{/|\\\\}}libarclite_macosx.a
// XCRUN_ARCLITE: -o {{[^ ]+}}

// RUN: %empty-directory(%t/DISTINCTIVE-PATH/usr/lib/arc)

// RUN: env PATH=%t/ANOTHER-DISTINCTIVE-PATH/usr/bin %t/DISTINCTIVE-PATH/usr/bin/swiftc -target x86_64-apple-macosx10.9 %s -### | %FileCheck -check-prefix=RELATIVE_ARCLITE %s

// RELATIVE_ARCLITE: bin{{/|\\\\}}ld
// RELATIVE_ARCLITE: {{/|\\\\}}DISTINCTIVE-PATH{{/|\\\\}}usr{{/|\\\\}}lib{{/|\\\\}}arc{{/|\\\\}}libarclite_macosx.a
// RELATIVE_ARCLITE: -o {{[^ ]+}}

// LINUX-STDLIB-RPATH: -Xlinker -rpath -Xlinker [[STDLIB_PATH:[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)linux]]
// LINUX-NO-STDLIB-RPATH-NOT: -Xlinker -rpath -Xlinker [[STDLIB_PATH:[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)linux]]

// ANDROID-STDLIB-RPATH: -Xlinker -rpath -Xlinker [[STDLIB_PATH:[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)android]]
// ANDROID-NO-STDLIB-RPATH-NOT: -Xlinker -rpath -Xlinker [[STDLIB_PATH:[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)android]]

// Clean up the test executable because hard links are expensive.
// RUN: rm -rf %t/DISTINCTIVE-PATH/usr/bin/swiftc
