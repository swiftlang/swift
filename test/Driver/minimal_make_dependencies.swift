// Test that we emit make-style dependencies with a minimal structure relative
// to the number of output files. That is, only one target should have the
// entire dependency structure listed. All other targets should delegate to it.
// As this blessed target is dependent on the compilation mode, we test each
// combination in turn.

// Try WMO - we need to emit targets for each primary object file.
// RUN: %empty-directory(%t)
// RUN: echo "{\"%/s\": {\"llvm-bc\": \"%/t/minimal_make_dependencies.bc\", \"object\": \"%/t/minimal_make_dependencies.o\"}, \"%/S/Inputs/main.swift\": {\"llvm-bc\": \"%/t/main.bc\", \"object\": \"%/t/main.o\"}}" > %t/ofmo.json
// RUN: cd %t && env TMPDIR="%t/tmp/" %target-swiftc_driver -module-name=ThisModule -wmo %S/Inputs/main.swift %s -emit-dependencies -Xfrontend -compress-make-style-dependencies -output-file-map %t/ofmo.json -c
// RUN: cat %t/*.d | %FileCheck -check-prefix=DEPENDENCIES-WMO %s

// DEPENDENCIES-WMO: swiftc : {{.*[/\\]}}Inputs{{[/\\]}}main.swift {{.*[/\\]}}minimal_make_dependencies.swift

// Try batch - we have one deps file per source file, but we should still only
// see at most two targets with the full dependency list.
// RUN: %empty-directory(%t)
// RUN: echo "{\"%/s\": {\"llvm-bc\": \"%/t/minimal_make_dependencies.bc\", \"object\": \"%/t/minimal_make_dependencies.o\"}, \"%/S/Inputs/main.swift\": {\"llvm-bc\": \"%/t/main.bc\", \"object\": \"%/t/main.o\"}, \"\": {\"swift-dependencies\": \"./main~buildrecord.swiftdeps\"}}" > %t/ofmo.json
// RUN: cd %t && env TMPDIR="%t/tmp/" %target-swiftc_driver -module-name=ThisModule -enable-batch-mode -incremental %S/Inputs/main.swift %s -emit-dependencies -Xfrontend -compress-make-style-dependencies -output-file-map %t/ofmo.json -c
// RUN: cat %t/*.d | %FileCheck -check-prefix=DEPENDENCIES-BATCH %s

// DEPENDENCIES-BATCH: swiftc : {{.*[/\\]}}Inputs{{[/\\]}}main.swift {{.*[/\\]}}minimal_make_dependencies.swift
// DEPENDENCIES-BATCH: swiftc : {{.*[/\\]}}Inputs{{[/\\]}}main.swift {{.*[/\\]}}minimal_make_dependencies.swift

// Try incremental non-batch - Same thing as batch mode: one deps file per
// object file and at most two targets with the full dependency list.
// RUN: %empty-directory(%t)
// RUN: echo "{\"%/s\": {\"llvm-bc\": \"%/t/minimal_make_dependencies.bc\", \"object\": \"%/t/minimal_make_dependencies.o\"}, \"%/S/Inputs/main.swift\": {\"llvm-bc\": \"%/t/main.bc\", \"object\": \"%/t/main.o\"}, \"\": {\"swift-dependencies\": \"./main~buildrecord.swiftdeps\"}}" > %t/ofmo.json
// RUN: cd %t && env TMPDIR="%t/tmp/" %target-swiftc_driver -module-name=ThisModule -incremental %S/Inputs/main.swift %s -emit-dependencies -Xfrontend -compress-make-style-dependencies -output-file-map %t/ofmo.json -c
// RUN: cat %t/*.d | %FileCheck -check-prefix=DEPENDENCIES-INCREMENTAL %s

// DEPENDENCIES-INCREMENTAL: swiftc : {{.*[/\\]}}Inputs{{[/\\]}}main.swift {{.*[/\\]}}minimal_make_dependencies.swift
// DEPENDENCIES-INCREMENTAL-NEXT: swiftc : {{.*[/\\]}}Inputs{{[/\\]}}main.swift {{.*[/\\]}}minimal_make_dependencies.swift

// Try merge-modules - we have one deps file for the big module, and the main
// target should be that .swiftmodule file.

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/modules
// RUN: %target-swift-frontend -emit-module -module-name ThisModule -o %t/modules/ThisModule_a.partial.swiftmodule -I -primary-file %S/Inputs/main.swift %s
// RUN: %target-swift-frontend -emit-module -module-name ThisModule -o %t/modules/ThisModule_b.partial.swiftmodule -I -primary-file %s %S/Inputs/main.swift
// RUN: %target-swift-frontend -merge-modules -emit-dependencies-path %t/mod.d -compress-make-style-dependencies -swift-version 4 -emit-module -module-name ThisModule -o %t/modules/ThisModule.swiftmodule %t/modules/ThisModule_a.partial.swiftmodule %t/modules/ThisModule_b.partial.swiftmodule
// RUN: cat %t/mod.d | %FileCheck -check-prefix=DEPENDENCIES-MERGE-MODULES %s

// DEPENDENCIES-MERGE-MODULES-DAG: swiftc : {{.*[/\\]}}modules{{[/\\]}}ThisModule_a.partial.swiftmodule {{.*[/\\]}}modules{{[/\\]}}ThisModule_b.partial.swiftmodule

// Try incremental with all the outputs - We should see one deps
// file per object file and all of the rest of the output files dependent on
// their main swift file.
// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/modules
// RUN: cd %t && env TMPDIR="%t/tmp/" %target-swift-frontend -c -enable-library-evolution -swift-version 5 -emit-dependencies -emit-dependencies-path %t/mod.d -compress-make-style-dependencies -emit-module-doc -emit-module-doc-path %t/ThisModule.swiftdoc -emit-module-interface-path %t/ThisModule.swiftinterface -emit-objc-header -emit-objc-header-path %t/ThisModule.h -emit-module -module-name ThisModule -emit-module-path %t/modules/ThisModule.swiftmodule %S/Inputs/main.swift %s
// RUN: cat %t/*.d | %FileCheck -check-prefix=DEPENDENCIES-ALL-THE-OUTPUTS %s

// DEPENDENCIES-ALL-THE-OUTPUTS: swiftc : {{.*[/\\]}}Inputs{{[/\\]}}main.swift {{.*[/\\]}}minimal_make_dependencies.swift

func libraryFunction() {}

