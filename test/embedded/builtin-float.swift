// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/include
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -target armv7em-none-none-eabi -emit-ir %t/Main.swift -enable-experimental-feature Embedded -module-cache-path %t/ModuleCache -Xcc -I%t/include

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: CODEGENERATOR=ARM
// REQUIRES: embedded_stdlib_cross_compiling
// REQUIRES: swift_feature_Embedded

// BEGIN Main.swift

print("hello")

// BEGIN include/stdint.h

#include <float.h>
typedef __INTPTR_TYPE__ intptr_t;
typedef __UINTPTR_TYPE__ uintptr_t;
typedef __INT64_TYPE__ int64_t;
typedef __UINT64_TYPE__ uint64_t;
typedef __INT32_TYPE__ int32_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __INT16_TYPE__ int16_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __INT8_TYPE__ int8_t;
typedef __UINT8_TYPE__ uint8_t;
