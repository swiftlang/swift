// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=Foo -F %S/Inputs/mock-sdk -module-cache-path %t/clang-module-cache -function-definitions=false -print-regular-comments > %t/Foo.printed.txt
// RUN: diff -u %t/Foo.printed.txt %S/Inputs/mock-sdk/Foo.printed.txt

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=Foo -F %S/Inputs/mock-sdk -module-cache-path %t/clang-module-cache -function-definitions=false -prefer-type-repr=true -module-print-submodules > %t/Foo.printed.recursive.txt
// RUN: diff -u %t/Foo.printed.recursive.txt %S/Inputs/mock-sdk/Foo.printed.recursive.txt

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=Foo.FooSub -F %S/Inputs/mock-sdk -module-cache-path %t/clang-module-cache -function-definitions=false -prefer-type-repr=true > %t/Foo.FooSub.printed.txt
// RUN: diff -u %t/Foo.FooSub.printed.txt %S/Inputs/mock-sdk/Foo.FooSub.printed.txt

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=FooHelper -F %S/Inputs/mock-sdk -module-cache-path %t/clang-module-cache -function-definitions=false -prefer-type-repr=true > %t/FooHelper.printed.txt
// RUN: diff -u %t/FooHelper.printed.txt %S/Inputs/mock-sdk/FooHelper.printed.txt

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=FooHelper.FooHelperSub -F %S/Inputs/mock-sdk -module-cache-path %t/clang-module-cache -function-definitions=false -prefer-type-repr=true > %t/FooHelper.FooHelperSub.printed.txt
// RUN: diff -u %t/FooHelper.FooHelperSub.printed.txt %S/Inputs/mock-sdk/FooHelper.FooHelperSub.printed.txt

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=FooHelper.FooHelperExplicit -F %S/Inputs/mock-sdk -module-cache-path %t/clang-module-cache -function-definitions=false -prefer-type-repr=true > %t/FooHelper.FooHelperExplicit.printed.txt
// FIXME: diff
// FIXME: we cannot yet print explicit submodules because we don't import them.

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=Foo -F %S/Inputs/mock-sdk -module-cache-path %t/clang-module-cache -function-definitions=false -prefer-type-repr=true -annotate-print > %t/Foo.annotated.txt
// RUN: diff -u %t/Foo.annotated.txt %S/Inputs/mock-sdk/Foo.annotated.txt


