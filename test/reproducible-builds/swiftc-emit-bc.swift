// RUN: %empty-directory(%t)
// NOTE: We need to re-use the output filename ("-o run.bc") between the two
//       runs since the LLVM BC file contains the filename specified:
//       <UnknownCode16 ... /> record string = 'run.bc'
// RUN: %target-build-swift -O -g -module-name foo -emit-bc %s -o %t/run.bc
// RUN: mv %t/run.bc %t/run-1.bc
// RUN: %target-build-swift -O -g -module-name foo -emit-bc %s -o %t/run.bc
// RUN: mv %t/run.bc %t/run-2.bc
// RUN: cmp %t/run-1.bc %t/run-2.bc
print("foo")
