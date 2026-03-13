// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: mkdir -p %t/cas

// A

// RUN: touch %t/A.swift
// RUN: %target-swift-frontend -emit-module -module-cache-path %t/clang-module-cache %t/A.swift -o %t/A.swiftmodule -I %S/../ScanDependencies/Inputs/Swift -swift-version 6 -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -O
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/A.swiftmodule > %t/A.casid

// B

// RUN: echo "import A" > %t/B.swift
// RUN: %target-swift-frontend -module-name B -scan-dependencies -module-cache-path %t/clang-module-cache %t/B.swift -o %t/B-deps.json -swift-version 6 -cache-compile-job -cas-path %t/cas %t/B.swift  -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -O -I %t

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/B-deps.json B> %t/B.cmd
// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/B-deps.json > %t/B-map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/B-map.json > %t/B-map.casid

// RUN: echo %t/B.swift > %t/inputs.FileList
// RUN: %target-swift-frontend-plain -emit-module -o %t/B.swiftmodule -g \
// RUN:   -cache-compile-job -cas-path %t/cas -swift-version 6 \
// RUN:   -module-name B -swift-module-file=A=@%t/A.casid \
// RUN:   -explicit-swift-module-map-file @%t/B-map.casid \
// RUN:   -filelist %t/inputs.FileList @%t/B.cmd -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib

// Main

// RUN: %target-swift-frontend -module-name Main -scan-dependencies -module-cache-path %t/clang-module-cache %s -o %t/deps.json -swift-version 6 -cache-compile-job -cas-path %t/cas %s  -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -O -I %t
// UN: %validate-json %t/deps.json | %FileCheck %s -DTEMP=%t

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Main> %t/Main.cmd
// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: rm %t/A.swiftmodule %t/A.swift
// RUN: echo %s > %t/inputs.FileList
// RUN: %target-swift-frontend-plain -emit-module -o %t/Main.swiftmodule -g \
// RUN:   -cache-compile-job -cas-path %t/cas -swift-version 6 \
// RUN:   -module-name Main -swift-module-file=B=@%t/B.casid \
// RUN:   -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -filelist %t/inputs.FileList @%t/Main.cmd -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib

import B
