// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies %t/project/Client.swift -o %t/deps.json \
// RUN:   -Xcc -working-directory -Xcc %t/project \
// RUN:   -Xcc -iquote -Xcc %t/project/Internal \
// RUN:   -Xcc -isystem -Xcc %t/project/SysHeaders \
// RUN:   -I %t/External
// RUN: %FileCheck %s --check-prefix=INTERNAL < %t/deps.json
// RUN: %FileCheck %s --check-prefix=SYSTEM < %t/deps.json
// RUN: %FileCheck %s --check-prefix=EXTERNAL < %t/deps.json

// INTERNAL: "modulePath": "{{.*}}Internal-{{.*}}.pcm"
// INTERNAL-NEXT: "libraryLevel": "ipi"

// SYSTEM: "modulePath": "{{.*}}SysHeaders-{{.*}}.pcm"
// SYSTEM-NEXT: "libraryLevel": "api"

// EXTERNAL: "modulePath": "{{.*}}External-{{.*}}.pcm"
// EXTERNAL-NEXT: "libraryLevel": "api"

//--- project/Client.swift
import Internal
import SysHeaders
import External

//--- project/Internal/module.modulemap
module Internal { header "Internal.h" }
//--- project/Internal/Internal.h
void internal(void);

//--- project/SysHeaders/module.modulemap
module SysHeaders { header "SysHeaders.h" }
//--- project/SysHeaders/SysHeaders.h
void sysHeaders(void);

//--- External/module.modulemap
module External { header "External.h" }
//--- External/External.h
void external(void);
