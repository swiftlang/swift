// RUN: %empty-directory(%t)


// Test with a single output file

// RUN: echo "single-threaded initial" >%t/log
// RUN: %target-swift-frontend -O -wmo %s %S/Inputs/simple.swift -module-name=test -c -o %t/test.o -Xllvm -debug-only=irgen 2>>%t/log

// CHECK-LABEL: single-threaded initial
// CHECK: test.o: MD5=[[TEST_MD5:[0-9a-f]+]]
// CHECK-NOT: prev MD5

// RUN: echo "single-threaded same compilation" >>%t/log
// RUN: %target-swift-frontend -O -wmo %s %S/Inputs/simple.swift -module-name=test -c -o %t/test.o -Xllvm -debug-only=irgen 2>>%t/log

// CHECK-LABEL: single-threaded same compilation
// CHECK: test.o: MD5=[[TEST_MD5]]
// CHECK: test.o: prev MD5=[[TEST_MD5]] skipping

// RUN: echo "single-threaded file changed" >>%t/log
// RUN: %target-swift-frontend -O -wmo %s %S/Inputs/simple2.swift -module-name=test -c -o %t/test.o -Xllvm -debug-only=irgen 2>>%t/log

// CHECK-LABEL: single-threaded file changed
// CHECK: test.o: MD5=[[TEST2_MD5:[0-9a-f]+]]
// CHECK: test.o: prev MD5=[[TEST_MD5]] recompiling

// RUN: echo "single-threaded option changed" >>%t/log
// RUN: %target-swift-frontend -O -wmo %s %S/Inputs/simple2.swift -disable-llvm-optzns -module-name=test -c -o %t/test.o -Xllvm -debug-only=irgen 2>>%t/log

// CHECK-LABEL: single-threaded option changed
// CHECK: test.o: MD5=[[TEST3_MD5:[0-9a-f]+]]
// CHECK: test.o: prev MD5=[[TEST2_MD5]] recompiling

// RUN: echo "empty file" >>%t/log
// RUN: %target-swift-frontend -c -primary-file %S/../Inputs/empty.swift -module-name=empty -o %t/empty.o -Xllvm -debug-only=irgen 2>>%t/log
// RUN: echo "empty file with profiling" >>%t/log
// RUN: %target-swift-frontend -c -primary-file %S/../Inputs/empty.swift -module-name=empty -o %t/empty.o -Xllvm -debug-only=irgen -profile-generate 2>>%t/log

// CHECK-LABEL: empty file
// CHECK: empty.o: MD5=[[EMPTY_MD5:[0-9a-f]+]]
// CHECK-LABEL: empty file with profiling
// CHECK: empty.o: prev MD5=[[EMPTY_MD5]] recompiling

// RUN: echo "empty file" >>%t/log
// RUN: %target-swift-frontend -c -primary-file %S/../Inputs/empty.swift -module-name=empty -o %t/empty.o -Xllvm -debug-only=irgen 2>>%t/log
// RUN: echo "empty file with fuzzer" >>%t/log
// RUN: %target-swift-frontend -c -primary-file %S/../Inputs/empty.swift -module-name=empty -o %t/empty.o -Xllvm -debug-only=irgen -sanitize=fuzzer 2>>%t/log

// CHECK-LABEL: empty file
// CHECK: empty.o: MD5=[[EMPTY_MD5_2:[0-9a-f]+]]
// CHECK-LABEL: empty file with fuzzer
// CHECK: empty.o: prev MD5=[[EMPTY_MD5_2]] recompiling

// Test with multiple output files

// RUN: echo "multi-threaded initial" >>%t/log
// RUN: %target-swift-frontend -O -wmo -num-threads 2 %s %S/Inputs/simple.swift -module-name=test -c -o %t/test.o -o %t/simple.o -Xllvm -debug-only=irgen 2>>%t/log

// CHECK-LABEL: multi-threaded initial
// CHECK-DAG: test.o: MD5=[[TEST4_MD5:[0-9a-f]+]]
// CHECK-DAG: test.o: prev MD5=[[TEST3_MD5]] recompiling
// CHECK-DAG: simple.o: MD5=[[SIMPLE_MD5:[0-9a-f]+]]

// RUN: echo "multi-threaded same compilation" >>%t/log
// RUN: %target-swift-frontend -O -wmo -num-threads 2 %s %S/Inputs/simple.swift -module-name=test -c -o %t/test.o -o %t/simple.o -Xllvm -debug-only=irgen 2>>%t/log

// CHECK-LABEL: multi-threaded same compilation
// CHECK-DAG: test.o: MD5=[[TEST4_MD5]]
// CHECK-DAG: test.o: prev MD5=[[TEST4_MD5]] skipping
// CHECK-DAG: simple.o: MD5=[[SIMPLE_MD5]]
// CHECK-DAG: simple.o: prev MD5=[[SIMPLE_MD5]] skipping

// RUN: echo "multi-threaded one file changed" >>%t/log
// RUN: %target-swift-frontend -O -wmo -num-threads 2 %s %S/Inputs/simple2.swift -module-name=test -c -o %t/test.o -o %t/simple.o -Xllvm -debug-only=irgen 2>>%t/log

// CHECK-LABEL: multi-threaded one file changed
// CHECK-DAG: test.o: MD5=[[TEST4_MD5]]
// CHECK-DAG: test.o: prev MD5=[[TEST4_MD5]] skipping
// CHECK-DAG: simple.o: MD5=[[SIMPLE2_MD5:[0-9a-f]+]]
// CHECK-DAG: simple.o: prev MD5=[[SIMPLE_MD5]] recompiling

// RUN: %FileCheck %s < %t/log

// REQUIRES: asserts

public func test_func1() {
  print("Hello")
}
