// RUN: not %target-swiftxx-frontend -emit-ir -Xcc -cxx-isystem -Xcc %S/Inputs/c++ -Xcc -stdlib=libc++ -Xcc -nostdinc++ -I %S/Inputs %s 2>&1 | %FileCheck %s

// REQUIRES: OS=windows-msvc

// CHECK: cannot load underlying module for 'CxxStdlib'
import CxxStdlib
