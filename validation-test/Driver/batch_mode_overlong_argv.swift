// REQUIRES: OS=macosx
// REQUIRES: no_asan
// RUN: %empty-directory(%t)
// RUN: cd %t && sh -c 'for i in $(seq 1 100); do for j in $(seq 1 10); do touch f_${i}_${j}.swift; done; done'
// RUN: mkdir -p ./additional/path/elements/often/make/filenames/longer/than/one/might/expect/especially/given/output/directories/deep/within/a/derived/data/folder/of/a/CI/machine/
// Force the repartitioning:
// RUN: cd %t && sh -c '%swiftc_driver -driver-show-job-lifecycle -driver-batch-size-limit 10000 -driver-force-one-batch-repartition -v -c -module-name foo -o ./additional/path/elements/often/make/filenames/longer/than/one/might/expect/especially/given/output/directories/deep/within/a/derived/data/folder/of/a/CI/machine/foo.o -emit-module -serialize-diagnostics -emit-dependencies -j 1 -enable-batch-mode ./f_*.swift >./out.txt 2>&1'
// RUN: %FileCheck %s <%t/out.txt
// CHECK-NOT: unable to execute command
// CHECK: Forming into 1 batches
// CHECK: Forming batch job from 1000 constituents
// CHECK: Forming into 2 batches
// CHECK: Forming batch job from 500 constituents
// CHECK: Forming batch job from 500 constituents
//
// Try it without the force; supplementary output file maps should obviate the repartition:
// RUN: cd %t && sh -c '%swiftc_driver -driver-show-job-lifecycle -driver-batch-size-limit 10000 -v -c -module-name foo -o ./additional/path/elements/often/make/filenames/longer/than/one/might/expect/especially/given/output/directories/deep/within/a/derived/data/folder/of/a/CI/machine/foo.o -emit-module -serialize-diagnostics -emit-dependencies -j 1 -enable-batch-mode ./f_*.swift >./out2.txt 2>&1'
// RUN: %FileCheck %s <%t/out2.txt -check-prefix=NO-REPARTITION
// NO-REPARTITION-NOT: unable to execute command
// NO-REPARTITION: Forming into 1 batches
// NO-REPARTITION: Forming batch job from 1000 constituents
// NO-REPARTITION-NOT: Forming into 2 batches
// NO-REPARTITION-NOT: Forming batch job from 500 constituents
// NO-REPARTITION-NOT: Forming batch job from 500 constituents

func thing() {
    print(1)
}
