// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/wrapped-modularization-error-remarks/A/A.swift -cxx-interoperability-mode=default -Xcc -fmodule-map-file=%S/Inputs/wrapped-modularization-error-remarks/CxxLib/include/module.modulemap -Xcc -I -Xcc %S/Inputs/wrapped-modularization-error-remarks/CxxLib/include -module-name A -o %t/A.swiftmodule
// RUN: %target-swift-frontend -c -primary-file %S/Inputs/wrapped-modularization-error-remarks/B/B.swift -cxx-interoperability-mode=default -I %t -Xcc -fmodule-map-file=%S/Inputs/wrapped-modularization-error-remarks/CxxLib/include/module.modulemap -Xcc -I -Xcc %S/Inputs/wrapped-modularization-error-remarks/CxxLib/include -module-name B -o %t/B.swift.o -Rmodule-recovery
// REQUIRES: OS=windows-msvc
