// A zippered and a plain target share a '-target' but depend on different
// Clang PCMs, so they must not share one '.swiftmodule'.

// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)

// RUN: %target-swift-frontend -parse-stdlib -scan-dependencies %s \
// RUN:   -module-cache-path %t/module-cache \
// RUN:   -I %S/Inputs/target-normalization/macOS \
// RUN:   -target arm64e-apple-macosx14.0 \
// RUN:   -o %t/plain.json

// RUN: %target-swift-frontend -parse-stdlib -scan-dependencies %s \
// RUN:   -module-cache-path %t/module-cache \
// RUN:   -I %S/Inputs/target-normalization/macOS \
// RUN:   -target arm64e-apple-macosx14.0 \
// RUN:   -target-variant arm64e-apple-ios15.0-macabi \
// RUN:   -o %t/zippered.json

// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/plain.json Zippered modulePath > %t/plain.txt
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/zippered.json Zippered modulePath > %t/zippered.txt
// RUN: not diff %t/plain.txt %t/zippered.txt

import Zippered
