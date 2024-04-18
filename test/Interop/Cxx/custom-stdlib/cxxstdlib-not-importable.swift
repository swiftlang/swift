// RUN: not %target-swiftxx-frontend -emit-ir -cxx-stdlib-path%S/Inputs/c++ -I %S/Inputs %s 2>&1 | %FileCheck %s

// CHECK: cannot load underlying module for 'CxxStdlib'
import CxxStdlib
