// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -module-name B -o %t/internal/B.swiftmodule -swift-version 5 \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -emit-module-interface-path %t/internal/B.swiftinterface -enable-library-evolution -I %t \
// RUN:   %t/B.swift

// RUN: %target-swift-frontend -emit-module -module-name A -o %t/testable/A.swiftmodule -swift-version 5 \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -emit-module-interface-path %t/testable/A.swiftinterface -enable-library-evolution -I %t/internal -enable-testing \
// RUN:   %t/A.swift

// RUN: %target-swift-frontend -emit-module -module-name A -o %t/regular/A.swiftmodule -swift-version 5 \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -emit-module-interface-path %t/regular/A.swiftinterface -enable-library-evolution -I %t/internal \
// RUN:   %t/A.swift

/// Import testable build, should use binary but no extra dependencies.
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-serialized -module-name Test %t/main.swift \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -o %t/deps1.json -I %t/testable -swift-version 5 -Rmodule-loading
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps1.json Test directDependencies | %FileCheck %s --check-prefix TEST1
// TEST1:  "swiftPrebuiltExternal": "A"
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps1.json A directDependencies | %FileCheck %s --check-prefix EMPTY --allow-empty
// EMPTY-NOT: B

/// Import regular build, should use binary.
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-serialized -module-name Test %t/main.swift \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -o %t/deps2.json -I %t/regular -swift-version 5 -Rmodule-loading
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps2.json Test directDependencies | %FileCheck %s --check-prefix TEST2
// TEST2:  "swiftPrebuiltExternal": "A"
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps2.json swiftPrebuiltExternal:A directDependencies | %FileCheck %s --check-prefix EMPTY --allow-empty

/// Testable import testable build, should use binary, even interface is preferred.
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -module-name Test %t/testable.swift \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -enable-testing \
// RUN:   -o %t/deps3.json -I %t/testable -I %t/internal -swift-version 5 -Rmodule-loading
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps3.json Test directDependencies | %FileCheck %s --check-prefix TEST3
// TEST3:  "swiftPrebuiltExternal": "A"
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps3.json swiftPrebuiltExternal:A directDependencies | %FileCheck %s --check-prefix TEST3-A
// TEST3-A:  "swift": "B"

/// Testable import sees non-testable module first, keep searching.
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -module-name Test %t/testable.swift \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -enable-testing \
// RUN:   -o %t/deps4.json -I %t/regular -I %t/testable -I %t/internal -swift-version 5 -Rmodule-loading 2>&1 | %FileCheck %s --check-prefix WARN
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps4.json Test directDependencies | %FileCheck %s --check-prefix TEST4
// TEST4:  "swiftPrebuiltExternal": "A"
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps4.json swiftPrebuiltExternal:A directDependencies | %FileCheck %s --check-prefix TEST4-A
// TEST4-A:  "swift": "B"
// WARN: warning: module file '{{.*}}{{/|\\}}A.swiftmodule' is incompatible with this Swift compiler: module built without '-enable-testing'

/// Testable import non-testable build enable testing, warning about the finding then error out.
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -module-name Test %t/testable.swift \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -enable-testing \
// RUN:   -o %t/deps5.json -I %t/regular -swift-version 5 -Rmodule-loading 2>&1 | %FileCheck %s --check-prefix ERROR
// ERROR: error: unable to resolve Swift module dependency to a compatible module: 'A'
// ERROR: note: found incompatible module '{{.*}}{{/|\\}}A.swiftmodule': module built without '-enable-testing'

/// Regular import a testable module with no interface, don't load optional dependencies.
// RUN: rm %t/testable/A.swiftinterface
// RUN: %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -module-name Test %t/main.swift \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -enable-testing \
// RUN:   -o %t/deps6.json -I %t/testable -swift-version 5 -Rmodule-loading
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps6.json Test directDependencies | %FileCheck %s --check-prefix TEST6
// TEST6:  "swiftPrebuiltExternal": "A"
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps6.json swiftPrebuiltExternal:A directDependencies | %FileCheck %s --check-prefix EMPTY --allow-empty


//--- main.swift
import A

//--- testable.swift
@testable import A

//--- A.swift
internal import B
@_spi(Testing) public func a() {}

//--- B.swift
public func b() {}

