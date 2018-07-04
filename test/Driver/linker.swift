// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s 2>&1 > %t.simple.txt
// RUN: %FileCheck %s < %t.simple.txt
// RUN: %FileCheck -check-prefix SIMPLE %s < %t.simple.txt

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 -static-stdlib %s 2>&1 > %t.simple.txt
// RUN: %FileCheck -check-prefix SIMPLE_STATIC -implicit-check-not -rpath %s < %t.simple.txt

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-ios7.1 %s 2>&1 > %t.simple.txt
// RUN: %FileCheck -check-prefix IOS_SIMPLE %s < %t.simple.txt

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-tvos9.0 %s 2>&1 > %t.simple.txt
// RUN: %FileCheck -check-prefix tvOS_SIMPLE %s < %t.simple.txt

// RUN: %swiftc_driver -driver-print-jobs -target i386-apple-watchos2.0 %s 2>&1 > %t.simple.txt
// RUN: %FileCheck -check-prefix watchOS_SIMPLE %s < %t.simple.txt

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-linux-gnu -Ffoo -Fsystem car -F cdr -framework bar -Lbaz -lboo -Xlinker -undefined %s 2>&1 > %t.linux.txt
// RUN: %FileCheck -check-prefix LINUX-x86_64 %s < %t.linux.txt

// RUN: %swiftc_driver -driver-print-jobs -target armv6-unknown-linux-gnueabihf -Ffoo -Fsystem car -F cdr -framework bar -Lbaz -lboo -Xlinker -undefined %s 2>&1 > %t.linux.txt
// RUN: %FileCheck -check-prefix LINUX-armv6 %s < %t.linux.txt

// RUN: %swiftc_driver -driver-print-jobs -target armv7-unknown-linux-gnueabihf -Ffoo -Fsystem car -F cdr -framework bar -Lbaz -lboo -Xlinker -undefined %s 2>&1 > %t.linux.txt
// RUN: %FileCheck -check-prefix LINUX-armv7 %s < %t.linux.txt

// RUN: %swiftc_driver -driver-print-jobs -target thumbv7-unknown-linux-gnueabihf -Ffoo -Fsystem car -F cdr -framework bar -Lbaz -lboo -Xlinker -undefined %s 2>&1 > %t.linux.txt
// RUN: %FileCheck -check-prefix LINUX-thumbv7 %s < %t.linux.txt

// RUN: %swiftc_driver -driver-print-jobs -target armv7-none-linux-androideabi -Ffoo -Fsystem car -F cdr -framework bar -Lbaz -lboo -Xlinker -undefined %s 2>&1 > %t.android.txt
// RUN: %FileCheck -check-prefix ANDROID-armv7 %s < %t.android.txt
// RUN: %FileCheck -check-prefix ANDROID-armv7-NEGATIVE %s < %t.android.txt

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-windows-cygnus -Ffoo -Fsystem car -F cdr -framework bar -Lbaz -lboo -Xlinker -undefined %s 2>&1 > %t.cygwin.txt
// RUN: %FileCheck -check-prefix CYGWIN-x86_64 %s < %t.cygwin.txt

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-windows-msvc -Ffoo -Fsystem car -F cdr -framework bar -Lbaz -lboo -Xlinker -undefined %s 2>&1 > %t.windows.txt
// RUN: %FileCheck -check-prefix WINDOWS-x86_64 %s < %t.windows.txt

// RUN: %swiftc_driver -driver-print-jobs -emit-library -target x86_64-unknown-linux-gnu %s -Lbar -o dynlib.out 2>&1 > %t.linux.dynlib.txt
// RUN: %FileCheck -check-prefix LINUX_DYNLIB-x86_64 %s < %t.linux.dynlib.txt

// RUN: %swiftc_driver -driver-print-jobs -emit-library -target x86_64-apple-macosx10.9.1 %s -sdk %S/../Inputs/clang-importer-sdk -lfoo -framework bar -Lbaz -Fgarply -Fsystem car -F cdr -Xlinker -undefined -Xlinker dynamic_lookup -o sdk.out 2>&1 > %t.complex.txt
// RUN: %FileCheck %s < %t.complex.txt
// RUN: %FileCheck -check-prefix COMPLEX %s < %t.complex.txt

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 -g %s | %FileCheck -check-prefix DEBUG %s

// RUN: %empty-directory(%t)
// RUN: touch %t/a.o
// RUN: touch %t/a.swiftmodule
// RUN: touch %t/b.o
// RUN: touch %t/b.swiftmodule
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s %t/a.o %t/a.swiftmodule %t/b.o %t/b.swiftmodule -o linker | %FileCheck -check-prefix LINK-SWIFTMODULES %s

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.10   %s > %t.simple-macosx10.10.txt
// RUN: %FileCheck %s < %t.simple-macosx10.10.txt
// RUN: %FileCheck -check-prefix SIMPLE %s < %t.simple-macosx10.10.txt

// RUN: %empty-directory(%t)
// RUN: touch %t/a.o
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s %t/a.o -o linker 2>&1 | %FileCheck -check-prefix COMPILE_AND_LINK %s
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s %t/a.o -driver-filelist-threshold=0 -o linker 2>&1 | %FileCheck -check-prefix FILELIST %s

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 -emit-library %s -module-name LINKER | %FileCheck -check-prefix INFERRED_NAME_DARWIN %s
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-linux-gnu -emit-library %s -module-name LINKER | %FileCheck -check-prefix INFERRED_NAME_LINUX %s
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-windows-cygnus -emit-library %s -module-name LINKER | %FileCheck -check-prefix INFERRED_NAME_WINDOWS %s
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-windows-msvc -emit-library %s -module-name LINKER | %FileCheck -check-prefix INFERRED_NAME_WINDOWS %s

// Here we specify an output file name using '-o'. For ease of writing these
// tests, we happen to specify the same file name as is inferred in the
// INFERRED_NAMED_DARWIN tests above: 'libLINKER.dylib'.
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 -emit-library %s -o libLINKER.dylib | %FileCheck -check-prefix INFERRED_NAME_DARWIN %s

// There are more RUN lines further down in the file.

// CHECK: swift
// CHECK: -o [[OBJECTFILE:.*]]

// CHECK-NEXT: bin/ld{{"? }}
// CHECK-DAG: [[OBJECTFILE]]
// CHECK-DAG: -L [[STDLIB_PATH:[^ ]+/lib/swift/macosx]]
// CHECK-DAG: -rpath [[STDLIB_PATH]]
// CHECK-DAG: -lSystem
// CHECK-DAG: -arch x86_64
// CHECK: -o {{[^ ]+}}


// SIMPLE: bin/ld{{"? }}
// SIMPLE-NOT: -syslibroot
// SIMPLE-DAG: -macosx_version_min 10.{{[0-9]+}}.{{[0-9]+}}
// SIMPLE-NOT: -syslibroot
// SIMPLE: -o linker


// SIMPLE_STATIC: swift
// SIMPLE_STATIC: -o [[OBJECTFILE:.*]]

// SIMPLE_STATIC-NEXT: bin/ld{{"? }}
// SIMPLE_STATIC: [[OBJECTFILE]]
// SIMPLE_STATIC: -lobjc
// SIMPLE_STATIC: -lSystem
// SIMPLE_STATIC: -arch x86_64
// SIMPLE_STATIC: -L [[STDLIB_PATH:[^ ]+/lib/swift_static/macosx]]
// SIMPLE_STATIC: -lc++
// SIMPLE_STATIC: -framework Foundation
// SIMPLE_STATIC: -force_load_swift_libs
// SIMPLE_STATIC: -macosx_version_min 10.{{[0-9]+}}.{{[0-9]+}}
// SIMPLE_STATIC: -no_objc_category_merging
// SIMPLE_STATIC: -o linker


// IOS_SIMPLE: swift
// IOS_SIMPLE: -o [[OBJECTFILE:.*]]

// IOS_SIMPLE: bin/ld{{"? }}
// IOS_SIMPLE-DAG: [[OBJECTFILE]]
// IOS_SIMPLE-DAG: -L {{[^ ]+/lib/swift/iphonesimulator}}
// IOS_SIMPLE-DAG: -lSystem
// IOS_SIMPLE-DAG: -arch x86_64
// IOS_SIMPLE-DAG: -ios_simulator_version_min 7.1.{{[0-9]+}}
// IOS_SIMPLE: -o linker


// tvOS_SIMPLE: swift
// tvOS_SIMPLE: -o [[OBJECTFILE:.*]]

// tvOS_SIMPLE: bin/ld{{"? }}
// tvOS_SIMPLE-DAG: [[OBJECTFILE]]
// tvOS_SIMPLE-DAG: -L {{[^ ]+/lib/swift/appletvsimulator}}
// tvOS_SIMPLE-DAG: -lSystem
// tvOS_SIMPLE-DAG: -arch x86_64
// tvOS_SIMPLE-DAG: -tvos_simulator_version_min 9.0.{{[0-9]+}}
// tvOS_SIMPLE: -o linker


// watchOS_SIMPLE: swift
// watchOS_SIMPLE: -o [[OBJECTFILE:.*]]

// watchOS_SIMPLE: bin/ld{{"? }}
// watchOS_SIMPLE-DAG: [[OBJECTFILE]]
// watchOS_SIMPLE-DAG: -L {{[^ ]+/lib/swift/watchsimulator}}
// watchOS_SIMPLE-DAG: -lSystem
// watchOS_SIMPLE-DAG: -arch i386
// watchOS_SIMPLE-DAG: -watchos_simulator_version_min 2.0.{{[0-9]+}}
// watchOS_SIMPLE: -o linker


// LINUX-x86_64: swift
// LINUX-x86_64: -o [[OBJECTFILE:.*]]

// LINUX-x86_64: clang++{{"? }}
// LINUX-x86_64-DAG: -pie
// LINUX-x86_64-DAG: [[OBJECTFILE]]
// LINUX-x86_64-DAG: -lswiftCore
// LINUX-x86_64-DAG: -L [[STDLIB_PATH:[^ ]+/lib/swift]]
// LINUX-x86_64-DAG: -Xlinker -rpath -Xlinker [[STDLIB_PATH]]
// LINUX-x86_64-DAG: -F foo -iframework car -F cdr
// LINUX-x86_64-DAG: -framework bar
// LINUX-x86_64-DAG: -L baz
// LINUX-x86_64-DAG: -lboo
// LINUX-x86_64-DAG: -Xlinker -undefined
// LINUX-x86_64: -o linker

// LINUX-armv6: swift
// LINUX-armv6: -o [[OBJECTFILE:.*]]

// LINUX-armv6: clang++{{"? }}
// LINUX-armv6-DAG: -pie
// LINUX-armv6-DAG: [[OBJECTFILE]]
// LINUX-armv6-DAG: -lswiftCore
// LINUX-armv6-DAG: -L [[STDLIB_PATH:[^ ]+/lib/swift]]
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

// LINUX-armv7: clang++{{"? }}
// LINUX-armv7-DAG: -pie
// LINUX-armv7-DAG: [[OBJECTFILE]]
// LINUX-armv7-DAG: -lswiftCore
// LINUX-armv7-DAG: -L [[STDLIB_PATH:[^ ]+/lib/swift]]
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

// LINUX-thumbv7: clang++{{"? }}
// LINUX-thumbv7-DAG: -pie
// LINUX-thumbv7-DAG: [[OBJECTFILE]]
// LINUX-thumbv7-DAG: -lswiftCore
// LINUX-thumbv7-DAG: -L [[STDLIB_PATH:[^ ]+/lib/swift]]
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

// ANDROID-armv7: clang++{{"? }}
// ANDROID-armv7-DAG: -pie
// ANDROID-armv7-DAG: [[OBJECTFILE]]
// ANDROID-armv7-DAG: -lswiftCore
// ANDROID-armv7-DAG: -L [[STDLIB_PATH:[^ ]+/lib/swift]]
// ANDROID-armv7-DAG: -target armv7-none-linux-androideabi
// ANDROID-armv7-DAG: -F foo -iframework car -F cdr
// ANDROID-armv7-DAG: -framework bar
// ANDROID-armv7-DAG: -L baz
// ANDROID-armv7-DAG: -lboo
// ANDROID-armv7-DAG: -Xlinker -undefined
// ANDROID-armv7: -o linker
// ANDROID-armv7-NEGATIVE-NOT: -Xlinker -rpath

// CYGWIN-x86_64: swift
// CYGWIN-x86_64: -o [[OBJECTFILE:.*]]

// CYGWIN-x86_64: clang++{{"? }}
// CYGWIN-x86_64-DAG: [[OBJECTFILE]]
// CYGWIN-x86_64-DAG: -lswiftCore
// CYGWIN-x86_64-DAG: -L [[STDLIB_PATH:[^ ]+/lib/swift]]
// CYGWIN-x86_64-DAG: -Xlinker -rpath -Xlinker [[STDLIB_PATH]]
// CYGWIN-x86_64-DAG: -F foo -iframework car -F cdr
// CYGWIN-x86_64-DAG: -framework bar
// CYGWIN-x86_64-DAG: -L baz
// CYGWIN-x86_64-DAG: -lboo
// CYGWIN-x86_64-DAG: -Xlinker -undefined
// CYGWIN-x86_64: -o linker

// WINDOWS-x86_64: swift
// WINDOWS-x86_64: -o [[OBJECTFILE:.*]]

// WINDOWS-x86_64: clang++{{"? }}
// WINDOWS-x86_64-DAG: [[OBJECTFILE]]
// WINDOWS-x86_64-DAG: -L [[STDLIB_PATH:[^ ]+/lib/swift/windows/x86_64]]
// WINDOWS-x86_64-DAG: -F foo -iframework car -F cdr
// WINDOWS-x86_64-DAG: -framework bar
// WINDOWS-x86_64-DAG: -L baz
// WINDOWS-x86_64-DAG: -lboo
// WINDOWS-x86_64-DAG: -Xlinker -undefined
// WINDOWS-x86_64: -o linker


// COMPLEX: bin/ld{{"? }}
// COMPLEX-DAG: -dylib
// COMPLEX-DAG: -syslibroot {{.*}}/Inputs/clang-importer-sdk
// COMPLEX-DAG: -lfoo
// COMPLEX-DAG: -framework bar
// COMPLEX-DAG: -L baz
// COMPLEX-DAG: -F garply -F car -F cdr
// COMPLEX-DAG: -undefined dynamic_lookup
// COMPLEX-DAG: -macosx_version_min 10.9.1
// COMPLEX: -o sdk.out

// LINUX_DYNLIB-x86_64: swift
// LINUX_DYNLIB-x86_64: -o [[OBJECTFILE:.*]]
// LINUX_DYNLIB-x86_64: -o [[AUTOLINKFILE:.*]]

// LINUX_DYNLIB-x86_64: clang++{{"? }}
// LINUX_DYNLIB-x86_64-DAG: -shared
// LINUX_DYNLIB-x86_64-DAG: -fuse-ld=gold
// LINUX_DYNLIB-x86_64-NOT: -pie
// LINUX_DYNLIB-x86_64-DAG: -Xlinker -rpath -Xlinker [[STDLIB_PATH:[^ ]+/lib/swift/linux]]
// LINUX_DYNLIB-x86_64: [[STDLIB_PATH]]/x86_64/swiftrt.o
// LINUX_DYNLIB-x86_64-DAG: [[OBJECTFILE]]
// LINUX_DYNLIB-x86_64-DAG: @[[AUTOLINKFILE]]
// LINUX_DYNLIB-x86_64-DAG: [[STDLIB_PATH]]
// LINUX_DYNLIB-x86_64-DAG: -lswiftCore
// LINUX_DYNLIB-x86_64-DAG: -L bar
// LINUX_DYNLIB-x86_64: -o dynlib.out

// DEBUG: bin/swift
// DEBUG-NEXT: bin/swift
// DEBUG-NEXT: bin/ld{{"? }}
// DEBUG: -add_ast_path {{.*}}/{{[^/]+}}.swiftmodule
// DEBUG: -o linker
// DEBUG-NEXT: {{^|bin/}}dsymutil
// DEBUG: linker
// DEBUG: -o linker.dSYM

// LINK-SWIFTMODULES: bin/swift
// LINK-SWIFTMODULES-NEXT: bin/ld{{"? }}
// LINK-SWIFTMODULES-SAME: -add_ast_path {{.*}}/a.swiftmodule
// LINK-SWIFTMODULES-SAME: -add_ast_path {{.*}}/b.swiftmodule
// LINK-SWIFTMODULES-SAME: -o linker

// COMPILE_AND_LINK: bin/swift
// COMPILE_AND_LINK-NOT: /a.o
// COMPILE_AND_LINK: linker.swift
// COMPILE_AND_LINK-NOT: /a.o
// COMPILE_AND_LINK-NEXT: bin/ld{{"? }}
// COMPILE_AND_LINK-DAG: /a.o
// COMPILE_AND_LINK-DAG: .o
// COMPILE_AND_LINK: -o linker


// FILELIST: bin/ld{{"? }}
// FILELIST-NOT: .o
// FILELIST: -filelist {{"?[^-]}}
// FILELIST-NOT: .o
// FILELIST: /a.o
// FILELIST-NOT: .o
// FILELIST: -o linker


// INFERRED_NAME_DARWIN: bin/swift
// INFERRED_NAME_DARWIN: -module-name LINKER
// INFERRED_NAME_DARWIN: bin/ld{{"? }}
// INFERRED_NAME_DARWIN:  -o libLINKER.dylib
// INFERRED_NAME_LINUX:   -o libLINKER.so
// INFERRED_NAME_WINDOWS: -o LINKER.dll


// Test ld detection. We use hard links to make sure
// the Swift driver really thinks it's been moved.

// RUN: rm -rf %t
// RUN: %empty-directory(%t/DISTINCTIVE-PATH/usr/bin)
// RUN: touch %t/DISTINCTIVE-PATH/usr/bin/ld
// RUN: chmod +x %t/DISTINCTIVE-PATH/usr/bin/ld
// RUN: %hardlink-or-copy(from: %swift_driver_plain, to: %t/DISTINCTIVE-PATH/usr/bin/swiftc)
// RUN: %t/DISTINCTIVE-PATH/usr/bin/swiftc -target x86_64-apple-macosx10.9 %s -### | %FileCheck -check-prefix=RELATIVE-LINKER %s

// RELATIVE-LINKER: /DISTINCTIVE-PATH/usr/bin/swift
// RELATIVE-LINKER: /DISTINCTIVE-PATH/usr/bin/ld
// RELATIVE-LINKER: -o {{[^ ]+}}

// Also test arclite detection. This uses xcrun to find arclite when it's not
// next to Swift.

// RUN: %empty-directory(%t/ANOTHER-DISTINCTIVE-PATH/usr/bin)
// RUN: %empty-directory(%t/ANOTHER-DISTINCTIVE-PATH/usr/lib/arc)
// RUN: cp %S/Inputs/xcrun-return-self.sh %t/ANOTHER-DISTINCTIVE-PATH/usr/bin/xcrun

// RUN: env PATH=%t/ANOTHER-DISTINCTIVE-PATH/usr/bin %t/DISTINCTIVE-PATH/usr/bin/swiftc -target x86_64-apple-macosx10.9 %s -### | %FileCheck -check-prefix=XCRUN_ARCLITE %s

// XCRUN_ARCLITE: bin/ld{{"? }}
// XCRUN_ARCLITE: /ANOTHER-DISTINCTIVE-PATH/usr/lib/arc/libarclite_macosx.a
// XCRUN_ARCLITE: -o {{[^ ]+}}

// RUN: %empty-directory(%t/DISTINCTIVE-PATH/usr/lib/arc)

// RUN: env PATH=%t/ANOTHER-DISTINCTIVE-PATH/usr/bin %t/DISTINCTIVE-PATH/usr/bin/swiftc -target x86_64-apple-macosx10.9 %s -### | %FileCheck -check-prefix=RELATIVE_ARCLITE %s

// RELATIVE_ARCLITE: bin/ld{{"? }}
// RELATIVE_ARCLITE: /DISTINCTIVE-PATH/usr/lib/arc/libarclite_macosx.a
// RELATIVE_ARCLITE: -o {{[^ ]+}}


// Clean up the test executable because hard links are expensive.
// RUN: rm -rf %t/DISTINCTIVE-PATH/usr/bin/swiftc

