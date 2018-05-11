// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name def_class -emit-module-path %t/def_class.1.swiftmodule %S/Inputs/def_class.swift -disable-objc-attr-requires-foundation-module -enable-objc-interop
// RUN: %target-swift-frontend -module-name def_class -emit-module-path %t/def_class.2.swiftmodule %S/Inputs/def_class.swift -disable-objc-attr-requires-foundation-module -enable-objc-interop
// RUN: diff <(llvm-bcanalyzer -dump %t/def_class.1.swiftmodule | sed -e 's/\.[0-9]\.swiftmodule/\.x\.swiftmodule/g') <(llvm-bcanalyzer -dump %t/def_class.2.swiftmodule | sed -e 's/\.[0-9]\.swiftmodule/\.x\.swiftmodule/g')

// Compiling the same set of files twice, without modifying them (and without
// generating inlinable SIL) should produce the same swiftmodule. We don't
// promise more than that at this time...

// RUN: %S/../Inputs/getmtime.py %t/def_class.1.swiftmodule > %t/orig-mtime.txt
// RUN: %target-swift-frontend -module-name def_class -emit-module-path %t/def_class.1.swiftmodule %S/Inputs/def_class.swift -disable-objc-attr-requires-foundation-module -enable-objc-interop
// RUN: diff %t/orig-mtime.txt <(%S/../Inputs/getmtime.py %t/def_class.1.swiftmodule)

// We shouldn't re-emit the module if it hasn't changed.
