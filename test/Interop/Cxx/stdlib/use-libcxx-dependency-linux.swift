// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Make sure we can build a Swift library with libc++ and then import it from a Swift executable.
// RUN: %target-build-swift %t/library.swift -emit-module -emit-library -cxx-interoperability-mode=default -Xcc -stdlib=libc++ -module-name MyLibrary -emit-module-path %t/artifacts/MyLibrary.swiftmodule -I %S/Inputs
// RUN: %target-build-swift %t/executable.swift -emit-executable -cxx-interoperability-mode=default -Xcc -stdlib=libc++ -module-name ImportsMyLibrary -I %t/artifacts -I %S/Inputs

// RUN: %empty-directory(%t/artifacts)

// Make sure Swift does not allow importing a library built with libc++ into an executable that uses libstdc++.
// RUN: %target-build-swift %t/library.swift -emit-module -emit-library -cxx-interoperability-mode=default -Xcc -stdlib=libc++ -module-name MyLibrary -emit-module-path %t/artifacts/MyLibrary.swiftmodule -I %S/Inputs
// RUN: not %target-build-swift %t/executable.swift -emit-executable -cxx-interoperability-mode=default -module-name ImportsMyLibrary -I %t/artifacts -I %S/Inputs 2>&1 | %FileCheck %s --check-prefix=CHECK-LIBCXX-DEPENDENCY

// CHECK-LIBCXX-DEPENDENCY: error: module 'MyLibrary' was built with libc++, but current compilation uses libstdc++

// RUN: %empty-directory(%t/artifacts)

// Make sure Swift does not allow importing a library built with libstdc++ into an executable that uses libc++.
// RUN: %target-build-swift %t/library.swift -emit-module -emit-library -cxx-interoperability-mode=default -module-name MyLibrary -emit-module-path %t/artifacts/MyLibrary.swiftmodule -I %S/Inputs
// RUN: not %target-build-swift %t/executable.swift -emit-executable -cxx-interoperability-mode=default -Xcc -stdlib=libc++ -module-name ImportsMyLibrary -I %t/artifacts -I %S/Inputs 2>&1 | %FileCheck %s --check-prefix=CHECK-LIBSTDCXX-DEPENDENCY

// CHECK-LIBSTDCXX-DEPENDENCY: error: module 'MyLibrary' was built with libstdc++, but current compilation uses libc++

// REQUIRES: OS=linux-gnu
// REQUIRES: system_wide_libcxx

//--- library.swift
import CxxStdlib
import StdString
public func getStdString() -> std.string { return std.string() }

//--- executable.swift
import MyLibrary
print("")
