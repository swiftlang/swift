// Emit the explicit system module.
// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-pcm -module-name script -Xcc -Xclang -Xcc -emit-module -Xcc -Xclang -Xcc -fsystem-module -o %t/script.pcm %S/Inputs/custom-modules/module.modulemap

// Verify that the input modulemap if marked [System] in the output of the -dump-pcm action.
// RUN: %swift-dump-pcm %t/script.pcm | %FileCheck %s --check-prefix=CHECK-SYSTEM-INPUT
// CHECK-SYSTEM-INPUT: Input file: {{.*[/\\]}}ClangImporter{{/|\\}}Inputs{{/|\\}}custom-modules{{/|\\}}module.modulemap [System]
import script
