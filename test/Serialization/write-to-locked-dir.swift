// RUN: test ! -d %t || chmod +w %t
// RUN: rm -rf %t && mkdir -p %t
// RUN: touch %t/main.swiftmodule %t/main.swiftdoc
// RUN: chmod -w %t

// RUN: %target-swift-frontend -emit-module -emit-module-doc -parse-stdlib -o %t -module-name main %s || chmod +w %t

// This is not combined with the previous chmod because of pipefail mode.
// RUN: chmod +w %t
// RUN: test -s %t/main.swiftmodule
// RUN: test -s %t/main.swiftdoc
