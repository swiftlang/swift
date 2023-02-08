// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -I %t -c -index-system-modules -index-store-path %t/store -enable-experimental-cxx-interop
// RUN: ls %t/store/interfaces | %FileCheck --check-prefix=FILES %s

// REQUIRES: OS=macosx
// REQUIRES: cxx-interop-fixed-cf_options

//--- test.swift

import CxxStdlib
import Foundation

// FILES: std-{{.*}}.pcm.symbolicswiftinterface
// FILES-NOT: Foundation*
