
// Verify the fake frontends used by dependencies tests support -print-target-info

// RUN: %{python} %S/Inputs/update-dependencies.py %swift-dependency-tool -frontend -print-target-info | %FileCheck %s
// RUN: %{python} %S/Inputs/update-dependencies-bad.py %swift-dependency-tool -frontend -print-target-info | %FileCheck %s

// RUN: %{python} %S/Inputs/fake-build-for-bitcode.py -frontend -print-target-info | %FileCheck %s
// RUN: %{python} %S/Inputs/fake-build-whole-module.py -frontend -print-target-info | %FileCheck %s
// RUN: %{python} %S/Inputs/modify-non-primary-files.py -frontend -print-target-info | %FileCheck %s

// CHECK: {{"}}target{{": \{}}

