// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/textual)
// RUN: %empty-directory(%t/binary)
// RUN: %empty-directory(%t/module-cache)

// RUN: %target-swift-frontend -emit-module %s -module-name Foo -swift-version 5 -disable-implicit-concurrency-module-import -user-module-version 113.33.44.55.66.77 -emit-module-interface-path %t/textual/Foo.swiftinterface -enable-library-evolution -emit-module-path %t/binary/Foo.swiftmodule

// RUN: %FileCheck %s --check-prefix=INTERFACE-FLAG < %t/textual/Foo.swiftinterface

// RUN: %target-swift-ide-test -print-module-metadata -module-to-print Foo -I %t/textual -source-filename %s -module-cache-path %t/module-cache | %FileCheck %s --check-prefix=USER-MODULE-PRINT

// RUN: %target-swift-ide-test -print-module-metadata -module-to-print Foo -I %t/binary -source-filename %s | %FileCheck %s --check-prefix=USER-MODULE-PRINT

// RUN: rm %t/binary/Foo.swiftmodule
// RUN: %target-swift-frontend -compile-module-from-interface %t/textual/Foo.swiftinterface -o %t/binary/Foo.swiftmodule
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print Foo -I %t/binary -source-filename %s | %FileCheck %s --check-prefix=USER-MODULE-PRINT

// A fifth component that llvm::VersionTuple can't represent is ignored.
// RUN: %target-swift-frontend -emit-module %s -module-name Bar -swift-version 5 -disable-implicit-concurrency-module-import -user-module-version 1.2.3.4.5000 -emit-module-interface-path %t/textual/Bar.swiftinterface -enable-library-evolution -emit-module-path %t/binary/Bar.swiftmodule 2>&1 | %FileCheck %s --check-prefix=LARGE-SUBBUILD-WARNING

// Only the job that emits the module diagnoses the ignored component.
// RUN: %target-swift-frontend -typecheck %s -module-name Bar -swift-version 5 -user-module-version 1.2.3.4.5000 2>&1 | %FileCheck %s --allow-empty --check-prefix=NO-WARNING
// RUN: %target-swift-frontend -c -primary-file %s -module-name Bar -swift-version 5 -user-module-version 1.2.3.4.5000 -emit-module-path %t/Bar~partial.swiftmodule -o %t/Bar.o 2>&1 | %FileCheck %s --allow-empty --check-prefix=NO-WARNING

// RUN: %target-swift-ide-test -print-module-metadata -module-to-print Bar -I %t/textual -source-filename %s -module-cache-path %t/module-cache | %FileCheck %s --check-prefix=LARGE-SUBBUILD-PRINT
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print Bar -I %t/binary -source-filename %s | %FileCheck %s --check-prefix=LARGE-SUBBUILD-PRINT

// INTERFACE-FLAG: -user-module-version 113.33.44.55

// USER-MODULE-PRINT: user module version: 113.33.44.55.66{{$}}

// LARGE-SUBBUILD-WARNING: warning: fifth component of version '1.2.3.4.5000' is ignored: must be an integer in [0, 1023]

// NO-WARNING-NOT: warning:

// LARGE-SUBBUILD-PRINT: user module version: 1.2.3.4{{$}}
