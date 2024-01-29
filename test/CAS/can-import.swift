// rdar://119964830 Temporarily disabling in Linux
// UNSUPPORTED: OS=linux-gnu

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/include

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:A > %t/A.cmd
// RUN: %swift_frontend_plain @%t/A.cmd

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:B > %t/B.cmd
// RUN: %swift_frontend_plain @%t/B.cmd

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/SwiftShims.cmd
// RUN: %swift_frontend_plain @%t/SwiftShims.cmd

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Swift > %t/Swift.cmd
// RUN: %swift_frontend_plain @%t/Swift.cmd

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json Swift moduleCacheKey | tr -d '\n' > %t/Swift.key
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json clang:SwiftShims moduleCacheKey | tr -d '\n' > %t/Shims.key
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json clang:A moduleCacheKey | tr -d '\n' > %t/A.key
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json clang:B moduleCacheKey | tr -d '\n' > %t/B.key
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json MyApp > %t/MyApp.cmd

// RUN: echo "[{" > %/t/map.json
// RUN: echo "\"moduleName\": \"Swift\"," >> %/t/map.json
// RUN: echo "\"modulePath\": \"Swift.swiftmodule\"," >> %/t/map.json
// RUN: echo -n "\"moduleCacheKey\": " >> %/t/map.json
// RUN: cat %t/Swift.key >> %/t/map.json
// RUN: echo "," >> %/t/map.json
// RUN: echo "\"isFramework\": false" >> %/t/map.json
// RUN: echo "}," >> %/t/map.json
// RUN: echo "{" >> %/t/map.json
// RUN: echo "\"moduleName\": \"SwiftShims\"," >> %/t/map.json
// RUN: echo "\"clangModulePath\": \"SwiftShims.pcm\"," >> %/t/map.json
// RUN: echo -n "\"clangModuleCacheKey\": " >> %/t/map.json
// RUN: cat %t/Shims.key >> %/t/map.json
// RUN: echo "," >> %/t/map.json
// RUN: echo "\"isFramework\": false" >> %/t/map.json
// RUN: echo "}," >> %/t/map.json
// RUN: echo "{" >> %/t/map.json
// RUN: echo "\"moduleName\": \"A\"," >> %/t/map.json
// RUN: echo "\"clangModulePath\": \"A.pcm\"," >> %/t/map.json
// RUN: echo -n "\"clangModuleCacheKey\": " >> %/t/map.json
// RUN: cat %t/A.key >> %/t/map.json
// RUN: echo "," >> %/t/map.json
// RUN: echo "\"isFramework\": false" >> %/t/map.json
// RUN: echo "}," >> %/t/map.json
// RUN: echo "{" >> %/t/map.json
// RUN: echo "\"moduleName\": \"B\"," >> %/t/map.json
// RUN: echo "\"clangModulePath\": \"B.pcm\"," >> %/t/map.json
// RUN: echo -n "\"clangModuleCacheKey\": " >> %/t/map.json
// RUN: cat %t/B.key >> %/t/map.json
// RUN: echo "," >> %/t/map.json
// RUN: echo "\"isFramework\": false" >> %/t/map.json
// RUN: echo "}]" >> %/t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %target-swift-frontend \
// RUN:   -typecheck -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -module-name MyApp -explicit-swift-module-map-file @%t/map.casid \
// RUN:   %t/main.swift @%t/MyApp.cmd

//--- main.swift
#if canImport(A.Sub)
func a() {}
#endif

#if canImport(A.Missing)
import A.Missing
#endif

#if canImport(B)
func b() {}
#endif

func useA() {
  a()
  b()
}

//--- include/module.modulemap
module A {
  module Sub {
    header "sub.h"
    export *
  }
}

module B {
  header "B.h"
  export *
}

//--- include/sub.h
void notused(void);

//--- include/B.h
void notused2(void);
