// RUN: %empty-directory(%t)
// RUN: touch %t/file-01.swift %t/file-02.swift %t/file-03.swift %t/file-04.swift %t/file-05.swift
// RUN: touch %t/file-06.swift %t/file-07.swift %t/file-08.swift %t/file-09.swift %t/file-10.swift
// RUN: touch %t/file-11.swift %t/file-12.swift %t/file-13.swift %t/file-14.swift %t/file-15.swift
// RUN: touch %t/file-16.swift
//
// RUN: echo "[ {\"%/t/file-01.swift\" : 10}, {\"%/t/file-02.swift\" : 20}, {\"%/t/file-03.swift\" : 30}, {\"%/t/file-04.swift\" : 40}, {\"%/t/file-05.swift\" : 11}, {\"%/t/file-06.swift\" : 21}, {\"%/t/file-07.swift\" : 31}, {\"%/t/file-08.swift\" : 41}, {\"%/t/file-09.swift\" : 12}, {\"%/t/file-10.swift\" : 22}, {\"%/t/file-11.swift\" : 32}, {\"%/t/file-12.swift\" : 42}, {\"%/t/file-13.swift\" : 13}, {\"%/t/file-14.swift\" : 23}, {\"%/t/file-15.swift\" : 33}, {\"%/t/file-16.swift\" : 43} ]" > %t/hint.json
//
// RUN: %swiftc_driver -enable-batch-mode -driver-show-job-lifecycle -driver-skip-execution -driver-batch-weight-hint-file %t/hint.json -j 4 %t/file-01.swift %t/file-02.swift %t/file-03.swift %t/file-04.swift %t/file-05.swift %t/file-06.swift %t/file-07.swift %t/file-08.swift %t/file-09.swift %t/file-10.swift %t/file-11.swift %t/file-12.swift %t/file-13.swift %t/file-14.swift %t/file-15.swift %t/file-16.swift | %FileCheck %s -check-prefix=BATCHHINT
//
// BATCHHINT: Found 16 batchable jobs
// BATCHHINT: Forming into 4 batches
// BATCHHINT: BatchWeightHint:: Job=0  with Weight=4.300000e+01 is assigned to Partition=0
// BATCHHINT: BatchWeightHint:: Job=1  with Weight=4.200000e+01 is assigned to Partition=1
// BATCHHINT: BatchWeightHint:: Job=2  with Weight=4.100000e+01 is assigned to Partition=3
// BATCHHINT: BatchWeightHint:: Job=3  with Weight=4.000000e+01 is assigned to Partition=2
// BATCHHINT: BatchWeightHint:: Job=4  with Weight=3.300000e+01 is assigned to Partition=2
// BATCHHINT: BatchWeightHint:: Job=5  with Weight=3.200000e+01 is assigned to Partition=3
// BATCHHINT: BatchWeightHint:: Job=6  with Weight=3.100000e+01 is assigned to Partition=1
// BATCHHINT: BatchWeightHint:: Job=7  with Weight=3.000000e+01 is assigned to Partition=0
// BATCHHINT: BatchWeightHint:: Job=8  with Weight=2.300000e+01 is assigned to Partition=2
// BATCHHINT: BatchWeightHint:: Job=9  with Weight=2.200000e+01 is assigned to Partition=1
// BATCHHINT: BatchWeightHint:: Job=10  with Weight=2.100000e+01 is assigned to Partition=0
// BATCHHINT: BatchWeightHint:: Job=11  with Weight=2.000000e+01 is assigned to Partition=3
// BATCHHINT: BatchWeightHint:: Job=12  with Weight=1.300000e+01 is assigned to Partition=3
// BATCHHINT: BatchWeightHint:: Job=13  with Weight=1.200000e+01 is assigned to Partition=0
// BATCHHINT: BatchWeightHint:: Job=14  with Weight=1.100000e+01 is assigned to Partition=1
// BATCHHINT: BatchWeightHint:: Job=15  with Weight=1.000000e+01 is assigned to Partition=2
// BATCHHINT: BatchWeightHint:: Summary of partition weights after assignment:
// BATCHHINT: BatchWeightHint:: PartitionWeight[0]=1.060000e+02
// BATCHHINT: BatchWeightHint:: PartitionWeight[1]=1.060000e+02
// BATCHHINT: BatchWeightHint:: PartitionWeight[2]=1.060000e+02
// BATCHHINT: BatchWeightHint:: PartitionWeight[3]=1.060000e+02
// BATCHHINT: Adding {compile: {{file-16-.*}} <= file-16.swift} to batch 0
// BATCHHINT: Adding {compile: {{file-12-.*}} <= file-12.swift} to batch 1
// BATCHHINT: Adding {compile: {{file-08-.*}} <= file-08.swift} to batch 3
// BATCHHINT: Adding {compile: {{file-04-.*}} <= file-04.swift} to batch 2
// BATCHHINT: Adding {compile: {{file-15-.*}} <= file-15.swift} to batch 2
// BATCHHINT: Adding {compile: {{file-11-.*}} <= file-11.swift} to batch 3
// BATCHHINT: Adding {compile: {{file-07-.*}} <= file-07.swift} to batch 1
// BATCHHINT: Adding {compile: {{file-03-.*}} <= file-03.swift} to batch 0
// BATCHHINT: Adding {compile: {{file-14-.*}} <= file-14.swift} to batch 2
// BATCHHINT: Adding {compile: {{file-10-.*}} <= file-10.swift} to batch 1
// BATCHHINT: Adding {compile: {{file-06-.*}} <= file-06.swift} to batch 0
// BATCHHINT: Adding {compile: {{file-02-.*}} <= file-02.swift} to batch 3
// BATCHHINT: Adding {compile: {{file-13-.*}} <= file-13.swift} to batch 3
// BATCHHINT: Adding {compile: {{file-09-.*}} <= file-09.swift} to batch 0
// BATCHHINT: Adding {compile: {{file-05-.*}} <= file-05.swift} to batch 1
// BATCHHINT: Adding {compile: {{file-01-.*}} <= file-01.swift} to batch 2
// BATCHHINT: Forming batch job from 4 constituents
// BATCHHINT: Forming batch job from 4 constituents
// BATCHHINT: Forming batch job from 4 constituents
// BATCHHINT: Forming batch job from 4 constituents
// BATCHHINT: Adding batch job to task queue: {compile: file-03{{.*}} file-06{{.*}} file-09{{.*}} ... 1 more <= file-03.swift file-06.swift file-09.swift ... 1 more}
// BATCHHINT: Adding batch job to task queue: {compile: file-05{{.*}} file-07{{.*}} file-10{{.*}} ... 1 more <= file-05.swift file-07.swift file-10.swift ... 1 more}
// BATCHHINT: Adding batch job to task queue: {compile: file-01{{.*}} file-04{{.*}} file-14{{.*}} ... 1 more <= file-01.swift file-04.swift file-14.swift ... 1 more}
// BATCHHINT: Adding batch job to task queue: {compile: file-02{{.*}} file-08{{.*}} file-11{{.*}} ... 1 more <= file-02.swift file-08.swift file-11.swift ... 1 more}
