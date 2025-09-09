// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/deps)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck %t/clientWithInteropDep.swift -I %t/deps -cxx-interoperability-mode=default -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -module-cache-path %t/module-cache &> %t/interop_dep.txt
// RUN: cat %t/interop_dep.txt | %FileCheck %s -check-prefix=ENABLE-CHECK

// RUN: %target-swift-frontend -typecheck %t/clientNoInteropDep.swift -I %t/deps -cxx-interoperability-mode=default -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -module-cache-path %t/module-cache &> %t/no_interop_dep.txt
// RUN: cat %t/no_interop_dep.txt | %FileCheck %s -check-prefix=DISABLE-CHECK

// RUN: %target-swift-frontend -typecheck %t/clientDarwin.swift -I %t/deps -cxx-interoperability-mode=default -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -module-cache-path %t/module-cache &> %t/darwin_dep.txt
// RUN: cat %t/darwin_dep.txt | %FileCheck %s -check-prefix=DISABLE-CHECK

// ENABLE-CHECK: remark: loaded module 'CxxStdlib' (overlay for a clang dependency)
// DISABLE-CHECK-NOT: remark: loaded module 'CxxStdlib' (overlay for a clang dependency)

//--- deps/bar.h
#include "std_bar.h"
void bar(void);

//--- deps/std_bar.h
void std_bar(void);

//--- deps/module.modulemap
module std_Bar [system] {
  header "std_bar.h"
  export *
}

module Bar [system] {
  header "bar.h"
  export *    
}

//--- deps/Foo.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Foo -enable-library-evolution -Rmodule-loading
import Bar
public struct Foo1 {}

//--- deps/FooNoInterop.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name FooNoInterop -enable-library-evolution
// swift-module-flags-ignorable: -formal-cxx-interoperability-mode=off -Rmodule-loading
import Bar
public struct Foo2 {}

//--- deps/Darwin.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Darwin -enable-library-evolution
// swift-module-flags-ignorable: -Rmodule-loading
import Bar
public struct Foo2 {}

//--- clientWithInteropDep.swift
import Foo

//--- clientNoInteropDep.swift
import FooNoInterop

//--- clientDarwin.swift
import Darwin

