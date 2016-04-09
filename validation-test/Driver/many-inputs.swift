// RUN: rm -rf %t && mkdir %t

// This limit was chosen because multi-threaded compilation broke here on OS X
// at one point.
// RUN: for i in {1..1100}; do echo "public func foo$i() {}" > %t/$i.swift; echo "CHECK: foo$i" >> %t/check.txt; done

// We're not using %target-swift-build because we don't want to run into any
// limits *xcrun* has on arguments.
// RUN: %swiftc_driver -sdk %sdk -target %target-triple -force-single-frontend-invocation -emit-library %t/*.swift -o %t/libWMO
// RUN: nm %t/libWMO | FileCheck %t/check.txt

// RUN: %swiftc_driver -sdk %sdk -target %target-triple -force-single-frontend-invocation -num-threads 1 -emit-library %t/*.swift -o %t/libWMOThreaded
// RUN: nm %t/libWMOThreaded | FileCheck %t/check.txt

// This is very slow due to process overhead. It's also doing one file at a time
// because we don't have a good way for lit tests to claim more than one thread.
// But it's still important to check.
// RUN: %swiftc_driver -sdk %sdk -target %target-triple -emit-library %t/*.swift -o %t/libMultiFile
// RUN: nm %t/libMultiFile | FileCheck %t/check.txt

