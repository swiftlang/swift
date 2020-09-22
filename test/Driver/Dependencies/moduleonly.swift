// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/moduleonly/* %t
// RUN: touch -t 201801230045 %t/*.swift

// RUN: cd %t && %target-build-swift -emit-module -output-file-map ./output.json -incremental ./foo.swift ./bar.swift ./baz.swift -module-name testmodule -v 2>&1 | %FileCheck -check-prefix=CHECK1 %s
// RUN: test ! -f %t/buildrecord.swiftdeps
// RUN: test   -f %t/buildrecord.swiftdeps~moduleonly

// CHECK1-DAG: -primary-file ./foo.swift
// CHECK1-DAG: -primary-file ./bar.swift
// CHECK1-DAG: -primary-file ./baz.swift

// RUN: cd %t && %target-build-swift -c -emit-module -output-file-map ./output.json -incremental ./foo.swift ./bar.swift ./baz.swift -module-name testmodule -v 2>&1 | %FileCheck -check-prefix=CHECK2 %s
// RUN: test -f %t/buildrecord.swiftdeps
// RUN: test -f %t/buildrecord.swiftdeps~moduleonly

// CHECK2-DAG: -primary-file ./foo.swift
// CHECK2-DAG: -primary-file ./bar.swift
// CHECK2-DAG: -primary-file ./baz.swift

// RUN: cd %t && %target-build-swift -emit-module -output-file-map ./output.json -incremental ./foo.swift ./bar.swift ./baz.swift -module-name testmodule -v 2>&1 | %FileCheck -check-prefix=CHECK3 %s
// RUN: test -f %t/buildrecord.swiftdeps~moduleonly
// RUN: test -f %t/buildrecord.swiftdeps

// CHECK3-NOT: -primary-file ./foo.swift
// CHECK3-NOT: -primary-file ./bar.swift
// CHECK3-NOT: -primary-file ./baz.swift

// RUN: touch -t 201801230123 %t/bar.swift
// RUN: cd %t && %target-build-swift -emit-module -output-file-map ./output.json -incremental ./foo.swift ./bar.swift ./baz.swift -module-name testmodule -v 2>&1 | %FileCheck -check-prefix=CHECK4 %s

// CHECK4-NOT: -primary-file ./foo.swift
// CHECK4-NOT: -primary-file ./baz.swift
// CHECK4-DAG: -primary-file ./bar.swift

// RUN: touch -t 201801230145 %t/baz.swift
// RUN: cd %t && %target-build-swift -c -emit-module -output-file-map ./output.json -incremental ./foo.swift ./bar.swift ./baz.swift -module-name testmodule -v 2>&1 | %FileCheck -check-prefix=CHECK5 %s

// CHECK5-NOT: -primary-file ./foo.swift
// CHECK5-DAG: -primary-file ./bar.swift
// CHECK5-DAG: -primary-file ./baz.swift

// RUN: cd %t && %target-build-swift -emit-module -output-file-map ./output.json -incremental ./foo.swift ./bar.swift ./baz.swift -module-name testmodule -v 2>&1 | %FileCheck -check-prefix=CHECK6 %s

// CHECK6-NOT: -primary-file ./foo.swift
// CHECK6-NOT: -primary-file ./bar.swift
// CHECK6-NOT: -primary-file ./baz.swift


// '-c' (without '-emit-module') from clean environment.
//
// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/moduleonly/* %t
// RUN: touch -t 201801230045 %t/*.swift
// RUN: cd %t && %target-build-swift -c -g -output-file-map ./output.json -incremental ./foo.swift ./bar.swift ./baz.swift -module-name testmodule -v 2>&1
// RUN: test ! -f %t/buildrecord.swiftdeps~moduleonly
// RUN: test   -f %t/buildrecord.swiftdeps
// RUN: test ! -f %t/foo~partial.swiftmodule

// '-emit-library -g' (without '-emit-module') from clean environment.
//
// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/moduleonly/* %t
// RUN: touch -t 201801230045 %t/*.swift
// RUN: cd %t && %target-build-swift -emit-library -g -output-file-map ./output.json -incremental ./foo.swift ./bar.swift ./baz.swift -module-name testmodule -v 2>&1
// RUN: test ! -f %t/buildrecord.swiftdeps~moduleonly
// RUN: test   -f %t/buildrecord.swiftdeps
// RUN: test   -f %t/foo~partial.swiftmodule

// Ensure '-emit-module' and '-c -emit-module' emits identical 'swiftmodule' and 'swiftdoc' file.
//
// RUN: rm -f %t-moduleonly.swiftmodule
// RUN: rm -f %t-moduleonly.swiftdoc
// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/moduleonly/* %t
// RUN: touch -t 201801230045 %t/*.swift
// RUN: cd %t && %target-build-swift -emit-module -output-file-map ./output.json -incremental ./foo.swift ./bar.swift ./baz.swift -module-name testmodule -v 2>&1
// RUN: cp -f %t/testmodule.swiftmodule %t-moduleonly.swiftmodule
// RUN: cp -f %t/testmodule.swiftdoc %t-moduleonly.swiftdoc
// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/moduleonly/* %t
// RUN: touch -t 201801230045 %t/*.swift
// RUN: cd %t && %target-build-swift -c -emit-module -output-file-map ./output.json -incremental ./foo.swift ./bar.swift ./baz.swift -module-name testmodule -v 2>&1
// RUN: diff %t/testmodule.swiftmodule %t-moduleonly.swiftmodule
// RUN: diff %t/testmodule.swiftdoc %t-moduleonly.swiftdoc
