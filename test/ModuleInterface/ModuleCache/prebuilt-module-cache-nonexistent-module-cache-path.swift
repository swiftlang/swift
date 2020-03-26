// RUN: %empty-directory(%t)

// Create a prebuilt module cache and populate it with a prebuilt module.
// RUN: %empty-directory(%t/prebuilt-cache)
// RUN: %target-swift-frontend -parse-stdlib %S/Inputs/prebuilt-module-cache/Lib.swiftinterface -emit-module-path %t/prebuilt-cache/Lib.swiftmodule - -module-name Lib

// Compile against the module with a module cache that does not exist
// RUN: %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/RandomPath/NonExistentCachePath -sdk %S/Inputs -I %S/Inputs/prebuilt-module-cache -prebuilt-module-cache-path %t/prebuilt-cache %s

// Make sure we installed a forwarding module.
// RUN: %{python} %S/Inputs/check-is-forwarding-module.py %t/RandomPath/NonExistentCachePath/Lib-*.swiftmodule

import Lib
