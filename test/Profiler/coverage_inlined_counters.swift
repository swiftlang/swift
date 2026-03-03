// RUN: %empty-directory(%t/out)
// RUN: split-file %s %t

// rdar://129337999 - Make sure we can link without issues.

// The legacy driver does not support doing this in one invocation, so split it
// up for compatibility with both the legacy and new driver.
// RUN: %target-build-swift -c %t/a.swift -profile-generate -profile-coverage-mapping -parse-as-library -module-name A -o %t/out/a.swift.o
// RUN: %target-build-swift -emit-module %t/a.swift -profile-generate -profile-coverage-mapping -parse-as-library -module-name A -emit-module-path %t/out/A.swiftmodule

// RUN: %target-build-swift %t/b.swift -profile-generate -profile-coverage-mapping -o %t/main -module-name B -I %t/out %t/out/a.swift.o

// Test again using only the old driver.
// RUN: %empty-directory(%t/out)
// RUN: env SWIFT_USE_OLD_DRIVER=1 %target-build-swift -c %t/a.swift -profile-generate -profile-coverage-mapping -parse-as-library -module-name A -o %t/out/a.swift.o
// RUN: env SWIFT_USE_OLD_DRIVER=1 %target-build-swift -emit-module %t/a.swift -profile-generate -profile-coverage-mapping -parse-as-library -module-name A -emit-module-path %t/out/A.swiftmodule
// RUN: env SWIFT_USE_OLD_DRIVER=1 %target-build-swift %t/b.swift -profile-generate -profile-coverage-mapping -o %t/main -module-name B -I %t/out %t/out/a.swift.o

// REQUIRES: profile_runtime
// UNSUPPORTED: OS=ios && CPU=arm64e

//--- a.swift
@_transparent
public func foo() {}

//--- b.swift
import A

foo()
