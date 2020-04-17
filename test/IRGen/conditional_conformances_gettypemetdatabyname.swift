// RUN: %target-swift-frontend -disable-generic-metadata-prespecialization -target x86_64-apple-macosx10.99 -emit-ir %S/../Inputs/conditional_conformance_basic_conformances.swift | %FileCheck %S/../Inputs/conditional_conformance_basic_conformances.swift --check-prefix=TYPEBYNAME
// RUN: %target-swift-frontend -prespecialize-generic-metadata -target x86_64-apple-macosx10.99 -emit-ir %S/../Inputs/conditional_conformance_basic_conformances.swift | %FileCheck %S/../Inputs/conditional_conformance_basic_conformances.swift --check-prefix=TYPEBYNAME_PRESPECIALIZED

// Too many pointer-sized integers in the IR
// REQUIRES: PTRSIZE=64
// REQUIRES: OS=macosx

