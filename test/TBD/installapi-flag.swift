// REQUIRES: VENDOR=apple 
// RUN: %empty-directory(%t)

// 1. Emit two TBDs, one with -tbd-is-installapi set, and one without

// RUN: %target-swift-frontend -typecheck %s -tbd-is-installapi -emit-tbd -emit-tbd-path %t/flag-provided.tbd
// RUN: %target-swift-frontend -typecheck %s -emit-tbd -emit-tbd-path %t/flag-omitted.tbd

// 2. Ensure that the file with -tbd-is-installapi passed includes the installapi flag

// RUN: %FileCheck %s --check-prefix FLAG-PROVIDED < %t/flag-provided.tbd

// 3. Ensure that the file without -tbd-is-installapi passed does not include the installapi flag

// RUN: %FileCheck %s --check-prefix FLAG-OMITTED < %t/flag-omitted.tbd

// FLAG-PROVIDED: installapi
// FLAG-OMITTED-NOT: installapi
