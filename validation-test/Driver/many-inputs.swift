// RUN: %empty-directory(%t)

// This limit was chosen because multi-threaded compilation broke here on OS X
// at one point.
// RUN: %{python} -c "for i in range(1, 1101): open(f'%t/{i}.swift', 'w').write(f'public func foo{i}() {{}}\n'); open(f'%t/check.txt', 'a').write(f'CHECK: foo{i}\n')"

// RUN: cd %t && %target-build-swift -whole-module-optimization -emit-library ./*.swift -o ./libWMO
// RUN: nm %t/libWMO | %FileCheck %t/check.txt

// RUN: cd %t && %target-build-swift -whole-module-optimization -num-threads 1 -emit-library ./*.swift -o ./libWMOThreaded
// RUN: nm %t/libWMOThreaded | %FileCheck %t/check.txt

// This is very slow due to process overhead. It's also doing one file at a time
// because we don't have a good way for lit tests to claim more than one thread.
// But it's still important to check.
// RUN: cd %t && %target-build-swift -emit-library ./*.swift -o ./libMultiFile
// RUN: nm %t/libMultiFile | %FileCheck %t/check.txt

// REQUIRES: long_test
// REQUIRES: executable_test

