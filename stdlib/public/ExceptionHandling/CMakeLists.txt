#===--- CMakeLists.txt - Concurrency support library ---------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2019 - 2022 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

set(swift_exception_handling_link_libraries
  swiftCore)

set(swift_exception_handling_c_compile_flags)
if(SWIFT_COMPILER_IS_MSVC_LIKE)
  list(APPEND swift_exception_handling_c_compile_flags
    /EHa
    /GR)
else()
  list(APPEND swift_exception_handling_c_compile_flags
    -fexceptions
    -frtti)
endif()

add_swift_target_library(swift_ExceptionHandling ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  ExceptionHandling.cpp
  ExceptionHandling.mm
  ExceptionHandling.swift

  SWIFT_MODULE_DEPENDS_LINUX Glibc
  SWIFT_MODULE_DEPENDS_FREEBSD Glibc
  SWIFT_MODULE_DEPENDS_OPENBSD Glibc
  SWIFT_MODULE_DEPENDS_CYGWIN Glibc
  SWIFT_MODULE_DEPENDS_HAIKU Glibc
  SWIFT_MODULE_DEPENDS_WINDOWS CRT

  LINK_LIBRARIES ${swift_exception_handling_link_libraries}

  C_COMPILE_FLAGS
    -Dswift_ExceptionHandling_EXPORTS
    ${swift_exception_handling_c_compile_flags}
  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    -parse-stdlib

  INSTALL_IN_COMPONENT stdlib
)
