// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name def_class -emit-module-path %t/def_class.1.swiftmodule %S/Inputs/def_class.swift -disable-objc-attr-requires-foundation-module -enable-objc-interop
// RUN: %target-swift-frontend -module-name def_class -emit-module-path %t/def_class.2.swiftmodule %S/Inputs/def_class.swift -disable-objc-attr-requires-foundation-module -enable-objc-interop
// RUN: llvm-bcanalyzer -dump %t/def_class.1.swiftmodule | sed -e 's/\.[0-9]\.swiftmodule/\.x\.swiftmodule/g' > %t.1
// RUN: llvm-bcanalyzer -dump %t/def_class.2.swiftmodule | sed -e 's/\.[0-9]\.swiftmodule/\.x\.swiftmodule/g' > %t.2
// RUN: cmp %t.1 %t.2

// Compiling the same set of files twice, without modifying them (and without
// generating inlinable SIL) should produce the same swiftmodule. We don't
// promise more than that at this time...

// RUN: %{python} %S/../Inputs/getmtime.py %t/def_class.1.swiftmodule > %t/orig-mtime.txt
// RUN: %target-swift-frontend -module-name def_class -emit-module-path %t/def_class.1.swiftmodule %S/Inputs/def_class.swift -disable-objc-attr-requires-foundation-module -enable-objc-interop
// RUN: %{python} %S/../Inputs/getmtime.py %t/def_class.1.swiftmodule > %t.3
// RUN: cmp %t/orig-mtime.txt %t.3

// We shouldn't re-emit the module if it hasn't changed.
