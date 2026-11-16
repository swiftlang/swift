// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir -p %t/strings %t/db
// RUN: cp %S/../diagnostics/Localization/Inputs/fr.strings %t/strings/fr.strings
// RUN: swift-serialize-diagnostics --input-file-path=%S/../diagnostics/Localization/Inputs/fr.strings --output-directory=%t/db/

// RUN: %target-swift-frontend -scan-dependencies -module-name Test \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/test.swift -o %t/deps-strings.json -cache-compile-job -cas-path %t/cas -scanner-prefix-map-paths %t /^test \
// RUN:   -locale fr -localization-path %t/strings
// RUN: %target-swift-frontend -scan-dependencies -module-name Test \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/test.swift -o %t/deps-db.json -cache-compile-job -cas-path %t/cas -scanner-prefix-map-paths %t /^test \
// RUN:   -locale fr -localization-path %t/db

/// Remove the localized messages so they can only be read from the CASFS.
// RUN: rm -rf %t/strings %t/db

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps-strings.json Test > %t/strings.cmd
// RUN: not %target-swift-frontend-plain -cache-compile-job -module-name Test -cas-path %t/cas @%t/strings.cmd \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -disable-implicit-swift-modules -parse-stdlib -emit-module -o %t/test.swiftmodule /^test/test.swift \
// RUN:   -locale fr -localization-path /^test/strings 2>&1 | %FileCheck %s

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps-db.json Test > %t/db.cmd
// RUN: not %target-swift-frontend-plain -cache-compile-job -module-name Test -cas-path %t/cas @%t/db.cmd \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -disable-implicit-swift-modules -parse-stdlib -emit-module -o %t/test.swiftmodule /^test/test.swift \
// RUN:   -locale fr -localization-path /^test/db 2>&1 | %FileCheck %s

// CHECK-NOT: specified localization directory
// CHECK-NOT: cannot find translations
// CHECK: error: chaîne non terminée littérale

//--- test.swift
_ = "HI!
