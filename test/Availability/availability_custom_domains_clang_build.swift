// REQUIRES: swift_feature_CustomAvailability
// XFAIL: OS=linux-android
// XFAIL: OS=linux-androideabi

// RUN: %empty-directory(%t)
// RUN: %target-clang -x c %S/Inputs/AvailabilityDomains.c -o %t/AvailabilityDomains.c.o -c
// RUN: %target-build-swift -emit-library %S/Inputs/custom_availability_file1.swift %S/Inputs/custom_availability_file2.swift -module-name main -enable-experimental-feature CustomAvailability -import-bridging-header %S/Inputs/AvailabilityDomains.h -Xlinker %t/AvailabilityDomains.c.o
// RUN: %target-build-swift -wmo -emit-library %S/Inputs/custom_availability_file1.swift %S/Inputs/custom_availability_file2.swift -module-name main -enable-experimental-feature CustomAvailability -import-bridging-header %S/Inputs/AvailabilityDomains.h -Xlinker %t/AvailabilityDomains.c.o
