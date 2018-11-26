// RUN: %empty-directory(%t)
// RUN: touch  %t/f_1_1.swift %t/f_1_2.swift %t/f_1_3.swift %t/f_1_4.swift %t/f_1_5.swift %t/f_1_6.swift %t/f_1_7.swift %t/f_1_8.swift %t/f_1_9.swift %t/f_1_10.swift
// RUN: touch  %t/f_2_1.swift %t/f_2_2.swift %t/f_2_3.swift %t/f_2_4.swift %t/f_2_5.swift %t/f_2_6.swift %t/f_2_7.swift %t/f_2_8.swift %t/f_2_9.swift %t/f_2_10.swift
// RUN: touch  %t/f_3_1.swift %t/f_3_2.swift %t/f_3_3.swift %t/f_3_4.swift %t/f_3_5.swift %t/f_3_6.swift %t/f_3_7.swift %t/f_3_8.swift %t/f_3_9.swift %t/f_3_10.swift
// RUN: touch  %t/f_4_1.swift %t/f_4_2.swift %t/f_4_3.swift %t/f_4_4.swift %t/f_4_5.swift %t/f_4_6.swift %t/f_4_7.swift %t/f_4_8.swift %t/f_4_9.swift %t/f_4_10.swift
// RUN: touch  %t/f_5_1.swift %t/f_5_2.swift %t/f_5_3.swift %t/f_5_4.swift %t/f_5_5.swift %t/f_5_6.swift %t/f_5_7.swift %t/f_5_8.swift %t/f_5_9.swift %t/f_5_10.swift
// RUN: touch  %t/f_6_1.swift %t/f_6_2.swift %t/f_6_3.swift %t/f_6_4.swift %t/f_6_5.swift %t/f_6_6.swift %t/f_6_7.swift %t/f_6_8.swift %t/f_6_9.swift %t/f_6_10.swift
// RUN: touch  %t/f_7_1.swift %t/f_7_2.swift %t/f_7_3.swift %t/f_7_4.swift %t/f_7_5.swift %t/f_7_6.swift %t/f_7_7.swift %t/f_7_8.swift %t/f_7_9.swift %t/f_7_10.swift
// RUN: touch  %t/f_8_1.swift %t/f_8_2.swift %t/f_8_3.swift %t/f_8_4.swift %t/f_8_5.swift %t/f_8_6.swift %t/f_8_7.swift %t/f_8_8.swift %t/f_8_9.swift %t/f_8_10.swift
// RUN: touch  %t/f_9_1.swift %t/f_9_2.swift %t/f_9_3.swift %t/f_9_4.swift %t/f_9_5.swift %t/f_9_6.swift %t/f_9_7.swift %t/f_9_8.swift %t/f_9_9.swift %t/f_9_10.swift
// RUN: touch  %t/f_10_1.swift %t/f_10_2.swift %t/f_10_3.swift %t/f_10_4.swift %t/f_10_5.swift %t/f_10_6.swift %t/f_10_7.swift %t/f_10_8.swift %t/f_10_9.swift %t/f_10_10.swift
// RUN: %swiftc_driver -driver-show-job-lifecycle -v -c -module-name foo -emit-module -serialize-diagnostics -emit-dependencies -j 1 -enable-batch-mode %t/f_*.swift >%t/out.txt 2>&1
// RUN: %FileCheck %s <%t/out.txt
// CHECK-NOT: unable to execute command
// CHECK: Forming into 4 batches
// CHECK: Forming batch job from 25 constituents
// CHECK: Forming batch job from 25 constituents
// CHECK: Forming batch job from 25 constituents
// CHECK: Forming batch job from 25 constituents
//
// RUN: %swiftc_driver -driver-show-job-lifecycle -driver-batch-size-limit 10 -v -c -module-name foo -emit-module -serialize-diagnostics -emit-dependencies -j 1 -enable-batch-mode %t/f_*.swift >%t/out.txt 2>&1
// RUN: %FileCheck %s <%t/out.txt -check-prefix=EXPLICIT-ARG
// EXPLICIT-ARG-NOT: unable to execute command
// EXPLICIT-ARG: Forming into 10 batches
// EXPLICIT-ARG: Forming batch job from 10 constituents
// EXPLICIT-ARG: Forming batch job from 10 constituents
// EXPLICIT-ARG: Forming batch job from 10 constituents
// EXPLICIT-ARG: Forming batch job from 10 constituents
// EXPLICIT-ARG: Forming batch job from 10 constituents
// EXPLICIT-ARG: Forming batch job from 10 constituents
// EXPLICIT-ARG: Forming batch job from 10 constituents
// EXPLICIT-ARG: Forming batch job from 10 constituents
// EXPLICIT-ARG: Forming batch job from 10 constituents
// EXPLICIT-ARG: Forming batch job from 10 constituents
