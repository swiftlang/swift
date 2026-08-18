// RUN: %empty-directory(%t)

// RUN: %target-build-swift -o %t/explicit-rst %S/Inputs/explicit.rst
// RUN: %target-codesign %t/explicit-rst
// RUN: %target-run %t/explicit-rst | %FileCheck %S/Inputs/explicit.rst

// RUN: %target-build-swift -o %t/indented-rst %S/Inputs/indented.rst
// RUN: %target-codesign %t/indented-rst
// RUN: %target-run %t/indented-rst | %FileCheck %S/Inputs/indented.rst

// RUN: %target-build-swift -o %t/fenced-md %S/Inputs/fenced.md
// RUN: %target-codesign %t/fenced-md
// RUN: %target-run %t/fenced-md | %FileCheck %S/Inputs/fenced.md

// RUN: %target-build-swift -o %t/indented-md %S/Inputs/indented.md
// RUN: %target-codesign %t/indented-md
// RUN: %target-run %t/indented-md | %FileCheck %S/Inputs/indented.md

// RUN: %target-build-swift -o %t/swiftenv-tex %S/Inputs/swiftenv.tex
// RUN: %target-codesign %t/swiftenv-tex
// RUN: %target-run %t/swiftenv-tex | %FileCheck %S/Inputs/swiftenv.tex

// REQUIRES: executable_test
