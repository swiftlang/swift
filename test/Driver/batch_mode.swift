// RUN: %empty-directory(%t)
// RUN: touch %t/file-01.swift %t/file-02.swift %t/file-03.swift %t/file-04.swift %t/file-05.swift
// RUN: touch %t/file-06.swift %t/file-07.swift %t/file-08.swift %t/file-09.swift %t/file-10.swift
// RUN: touch %t/file-11.swift %t/file-12.swift %t/file-13.swift %t/file-14.swift %t/file-15.swift
// RUN: touch %t/file-16.swift %t/file-17.swift %t/file-18.swift %t/file-19.swift %t/file-20.swift
// RUN: touch %t/file-21.swift %t/file-22.swift %t/file-23.swift %t/file-24.swift %t/file-25.swift
// RUN: touch %t/file-26.swift %t/file-27.swift %t/file-28.swift %t/file-29.swift %t/file-30.swift
//
// RUN: %swiftc_driver -enable-batch-mode -driver-show-job-lifecycle -driver-skip-execution -j 4 %t/file-01.swift %t/file-02.swift %t/file-03.swift %t/file-04.swift %t/file-05.swift %t/file-06.swift %t/file-07.swift %t/file-08.swift %t/file-09.swift %t/file-10.swift %t/file-11.swift %t/file-12.swift %t/file-13.swift %t/file-14.swift %t/file-15.swift %t/file-16.swift %t/file-17.swift %t/file-18.swift %t/file-19.swift %t/file-20.swift %t/file-21.swift %t/file-22.swift %t/file-23.swift %t/file-24.swift %t/file-25.swift %t/file-26.swift %t/file-27.swift %t/file-28.swift %t/file-29.swift %t/file-30.swift | %FileCheck %s -check-prefix=SEED0
//
// RUN: %swiftc_driver -enable-batch-mode -driver-show-job-lifecycle -driver-skip-execution -j 4 -driver-batch-seed 1 %t/file-01.swift %t/file-02.swift %t/file-03.swift %t/file-04.swift %t/file-05.swift %t/file-06.swift %t/file-07.swift %t/file-08.swift %t/file-09.swift %t/file-10.swift %t/file-11.swift %t/file-12.swift %t/file-13.swift %t/file-14.swift %t/file-15.swift %t/file-16.swift %t/file-17.swift %t/file-18.swift %t/file-19.swift %t/file-20.swift %t/file-21.swift %t/file-22.swift %t/file-23.swift %t/file-24.swift %t/file-25.swift %t/file-26.swift %t/file-27.swift %t/file-28.swift %t/file-29.swift %t/file-30.swift | %FileCheck %s -check-prefix=SEED1
//
// RUN: %swiftc_driver -enable-batch-mode -driver-show-job-lifecycle -driver-skip-execution -j 4 -driver-batch-seed 2 %t/file-01.swift %t/file-02.swift %t/file-03.swift %t/file-04.swift %t/file-05.swift %t/file-06.swift %t/file-07.swift %t/file-08.swift %t/file-09.swift %t/file-10.swift %t/file-11.swift %t/file-12.swift %t/file-13.swift %t/file-14.swift %t/file-15.swift %t/file-16.swift %t/file-17.swift %t/file-18.swift %t/file-19.swift %t/file-20.swift %t/file-21.swift %t/file-22.swift %t/file-23.swift %t/file-24.swift %t/file-25.swift %t/file-26.swift %t/file-27.swift %t/file-28.swift %t/file-29.swift %t/file-30.swift | %FileCheck %s -check-prefix=SEED2
//
// 30 files / 4 batches => 2 batches of 8 jobs + 2 batches of 7 jobs
//
// SEED0: Found 30 batchable jobs
// SEED0: Forming into 4 batches
// SEED0: Adding {compile: {{file-01-.*}} <= file-01.swift} to batch 0
// SEED0: Adding {compile: {{file-02-.*}} <= file-02.swift} to batch 0
// SEED0: Adding {compile: {{file-09-.*}} <= file-09.swift} to batch 1
// SEED0: Adding {compile: {{file-10-.*}} <= file-10.swift} to batch 1
// SEED0: Adding {compile: {{file-19-.*}} <= file-19.swift} to batch 2
// SEED0: Adding {compile: {{file-20-.*}} <= file-20.swift} to batch 2
// SEED0: Adding {compile: {{file-29-.*}} <= file-29.swift} to batch 3
// SEED0: Adding {compile: {{file-30-.*}} <= file-30.swift} to batch 3
// SEED0: Forming batch job from 8 constituents
// SEED0: Forming batch job from 8 constituents
// SEED0: Forming batch job from 7 constituents
// SEED0: Forming batch job from 7 constituents
// SEED0: Adding batch job to task queue: {compile: file-01{{.*}} file-02{{.*}} file-03{{.*}} ... 5 more <= file-01.swift file-02.swift file-03.swift ... 5 more}
// SEED0: Adding batch job to task queue: {compile: file-09{{.*}} file-10{{.*}} file-11{{.*}} ... 5 more <= file-09.swift file-10.swift file-11.swift ... 5 more}
// SEED0: Adding batch job to task queue: {compile: file-17{{.*}} file-18{{.*}} file-19{{.*}} ... 4 more <= file-17.swift file-18.swift file-19.swift ... 4 more}
// SEED0: Adding batch job to task queue: {compile: file-24{{.*}} file-25{{.*}} file-26{{.*}} ... 4 more <= file-24.swift file-25.swift file-26.swift ... 4 more}
//
// SEED1: Found 30 batchable jobs
// SEED1: Forming into 4 batches
// SEED1: Adding {compile: file-{{.*}} <= file-{{.*}}.swift} to batch 0
// SEED1: Adding {compile: file-{{.*}} <= file-{{.*}}.swift} to batch 1
// SEED1: Adding {compile: file-{{.*}} <= file-{{.*}}.swift} to batch 2
// SEED1: Adding {compile: file-{{.*}} <= file-{{.*}}.swift} to batch 3
// SEED1: Forming batch job from 8 constituents
// SEED1: Forming batch job from 8 constituents
// SEED1: Forming batch job from 7 constituents
// SEED1: Forming batch job from 7 constituents
// SEED1-NOT: Adding batch job to task queue: {compile: file-01{{.*}} file-02{{.*}} file-03{{.*}} ... 5 more <= file-01.swift file-02.swift file-03.swift ... 5 more }
// SEED1: Added to TaskQueue: {compile: {{.*}} <= {{file-[0-3][2-9].swift .*}}}
// SEED1: Added to TaskQueue: {compile: {{.*}} <= {{.*}}}
// SEED1: Added to TaskQueue: {compile: {{.*}} <= {{.*}}}
//
// SEED2: Found 30 batchable jobs
// SEED2: Forming into 4 batches
// SEED2: Adding {compile: file-{{.*}} <= file-{{.*}}.swift} to batch 0
// SEED2: Adding {compile: file-{{.*}} <= file-{{.*}}.swift} to batch 1
// SEED2: Adding {compile: file-{{.*}} <= file-{{.*}}.swift} to batch 2
// SEED2: Adding {compile: file-{{.*}} <= file-{{.*}}.swift} to batch 3
// SEED2: Forming batch job from 8 constituents
// SEED2: Forming batch job from 8 constituents
// SEED2: Forming batch job from 7 constituents
// SEED2: Forming batch job from 7 constituents
// SEED2-NOT: Adding batch job to task queue: {compile: file-01{{.*}} file-02{{.*}} file-03{{.*}} ... 5 more <= file-01.swift file-02.swift file-03.swift ... 5 more }
// SEED2: Added to TaskQueue: {compile: {{.*}} <= {{file-[0-3][2-9].swift .*}}}
// SEED2: Added to TaskQueue: {compile: {{.*}} <= {{.*}}}
// SEED2: Added to TaskQueue: {compile: {{.*}} <= {{.*}}}
