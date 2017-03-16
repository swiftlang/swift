// RUN: %swiftc_driver -typecheck -enable-bridging-pch -driver-print-actions -import-objc-header %S/Inputs/bridging-header.h %s 2>&1 | %FileCheck %s -check-prefix=YESPCHACT
// YESPCHACT: 0: input, "{{.*}}Inputs/bridging-header.h", objc-header
// YESPCHACT: 1: generate-pch, {0}, pch
// YESPCHACT: 2: input, "{{.*}}bridging-pch.swift", swift
// YESPCHACT: 3: compile, {2, 1}, none

// RUN: %swiftc_driver -typecheck -driver-print-actions -import-objc-header %S/Inputs/bridging-header.h %s 2>&1 | %FileCheck %s -check-prefix=NOPCHACT
// NOPCHACT: 0: input, "{{.*}}bridging-pch.swift", swift
// NOPCHACT: 1: compile, {0}, none

// RUN: %swiftc_driver -typecheck -enable-bridging-pch -driver-print-jobs -import-objc-header %S/Inputs/bridging-header.h %s 2>&1 | %FileCheck %s -check-prefix=YESPCHJOB
// YESPCHJOB: {{.*}}swift -frontend {{.*}} -emit-pch -o {{.*}}bridging-header-{{.*}}.pch
// YESPCHJOB: {{.*}}swift -frontend {{.*}} -import-objc-header {{.*}}bridging-header-{{.*}}.pch

// RUN: %swiftc_driver -typecheck -driver-print-jobs -import-objc-header %S/Inputs/bridging-header.h %s 2>&1 | %FileCheck %s -check-prefix=NOPCHJOB
// NOPCHJOB: {{.*}}swift -frontend {{.*}} -import-objc-header {{.*}}Inputs/bridging-header.h

// RUN: echo "{\"\": {\"swift-dependencies\": \"master.swiftdeps\"}}" > %t.json
// RUN: %swiftc_driver -typecheck -incremental -enable-bridging-pch -output-file-map %t.json -import-objc-header %S/Inputs/bridging-header.h %s
