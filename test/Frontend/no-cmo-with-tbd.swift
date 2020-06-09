// RUN: not %target-swift-frontend -O -parse-as-library -cross-module-optimization -emit-tbd -module-name=Test %s -c -o /dev/null -verify 2>&1 | %FileCheck %s

// CHECK: Test-Based InstallAPI (TBD) is not support with cross-module-optimization

func foo() { }
