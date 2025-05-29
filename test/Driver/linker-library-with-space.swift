// RUN: %swiftc_driver -sdk '""' -driver-print-jobs -target x86_64-unknown-linux-gnu -Ffoo -Fsystem car -F cdr -framework bar -Lbaz -lboo -Xlinker -undefined %s 2>&1 > %t.linux.txt
// RUN: %FileCheck -check-prefix LINUX-lib-flag-space %s < %t.linux.txt

// LINUX-lib-flag-space: swift
// LINUX-lib-flag-space: -o [[OBJECTFILE:.*]]

// LINUX-lib-flag-space: clang{{(\.exe)?"? }}
// LINUX-lib-flag-space-DAG: -pie
// LINUX-lib-flag-space-DAG: [[OBJECTFILE]]
// LINUX-lib-flag-space-DAG: -lswiftCore
// LINUX-lib-flag-space-DAG: -L [[STDLIB_PATH:[^ ]+(/|\\\\)lib(/|\\\\)swift(/|\\\\)]]
// LINUX-lib-flag-space-DAG: -Xlinker -rpath -Xlinker [[STDLIB_PATH]]
// LINUX-lib-flag-space-DAG: -F foo -iframework car -F cdr
// LINUX-lib-flag-space-DAG: -framework bar
// LINUX-lib-flag-space-DAG: -L baz
// LINUX-lib-flag-space-DAG: -lboo
// LINUX-lib-flag-space-DAG: -Xlinker -undefined
// LINUX-lib-flag-space: -o main
