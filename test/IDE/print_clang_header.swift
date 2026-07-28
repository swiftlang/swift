// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// REQUIRES: objc_interop

// RUN: echo '#include "header-to-print.h"' > %t.m
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -source-filename %s -print-header -header-to-print %S/Inputs/print_clang_header/header-to-print.h -enable-objc-interop -disable-objc-attr-requires-foundation-module --cc-args %target-cc-options -isysroot %clang-importer-sdk-path -fsyntax-only %t.m -I %S/Inputs/print_clang_header > %t.txt
// RUN: diff -u %S/Inputs/print_clang_header/header-to-print.h.printed.txt %t.txt

// RUN: %clang %target-cc-options -isysroot %clang-importer-sdk-path -fmodules -x objective-c-header %S/Inputs/print_clang_header/header-to-print.h -o %t.h.pch
// RUN: touch %t.empty.m
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -source-filename %s -print-header -header-to-print %S/Inputs/print_clang_header/header-to-print.h -enable-objc-interop -disable-objc-attr-requires-foundation-module --cc-args %target-cc-options -isysroot %clang-importer-sdk-path -fsyntax-only %t.empty.m -I %S/Inputs/print_clang_header -include %t.h > %t.with-pch.txt
// RUN: diff -u %S/Inputs/print_clang_header/header-to-print.h.command-line-include.printed.txt %t.with-pch.txt
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -source-filename %s -print-header -header-to-print %S/Inputs/print_clang_header/header-to-print.h -enable-objc-interop -disable-objc-attr-requires-foundation-module --cc-args %target-cc-options -isysroot %clang-importer-sdk-path -fsyntax-only %t.empty.m -I %S/Inputs/print_clang_header -include %S/Inputs/print_clang_header/header-to-print.h > %t.with-include.txt
// RUN: diff -u %S/Inputs/print_clang_header/header-to-print.h.command-line-include.printed.txt %t.with-include.txt

// RUN: echo '#include <Foo/header-to-print.h>' > %t.framework.m
// RUN: sed -e "s:INPUT_DIR:%S/Inputs/print_clang_header:g" -e "s:OUT_DIR:%t:g" %S/Inputs/print_clang_header/Foo-vfsoverlay.yaml > %t.yaml
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -source-filename %s -print-header -header-to-print %S/Inputs/print_clang_header/header-to-print.h -enable-objc-interop -disable-objc-attr-requires-foundation-module --cc-args %target-cc-options -isysroot %clang-importer-sdk-path -fsyntax-only %t.framework.m -F %t -ivfsoverlay %t.yaml -Xclang -fmodule-name=Foo > %t.framework.txt
// RUN: diff -u %S/Inputs/print_clang_header/header-to-print.h.printed.txt %t.framework.txt

// Test header interface printing from a clang module.
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -source-filename %s -print-header -header-to-print %S/Inputs/print_clang_header/header-to-print.h -enable-objc-interop -disable-objc-attr-requires-foundation-module --cc-args %target-cc-options -isysroot %clang-importer-sdk-path -fsyntax-only %t.framework.m -F %t -ivfsoverlay %t.yaml > %t.module.txt
// RUN: diff -u %S/Inputs/print_clang_header/header-to-print.h.module.printed.txt %t.module.txt

// Test header interface printing with ModernImportedCArrays enabled.
// RUN: echo '#include "header-to-print.h"' > %t.m
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -source-filename %s -print-header -header-to-print %S/Inputs/print_clang_header/header-to-print.h -enable-objc-interop -disable-objc-attr-requires-foundation-module -enable-experimental-feature ModernImportedCArraysOnly --cc-args %target-cc-options -isysroot %clang-importer-sdk-path -fsyntax-only %t.m -I %S/Inputs/print_clang_header > %t.modern-c-array.raw.txt
// RUN: sed -E -e "s:macOS|iOS|tvOS|watchOS|visionOS:PLATFORM:g" %t.modern-c-array.raw.txt > %t.modern-c-array.txt
// RUN: diff -u %S/Inputs/print_clang_header/header-to-print.h.modern-c-array.printed.txt %t.modern-c-array.txt

// REQUIRES: swift_feature_ModernImportedCArraysOnly
