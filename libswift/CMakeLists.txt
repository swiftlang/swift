# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for Swift project authors

# A dummy libswift if libswift is disabled
add_swift_host_library(libswiftStub OBJECT LibSwiftStubs.cpp)

if (NOT LIBSWIFT_BUILD_MODE)

  add_library(libswift ALIAS libswiftStub)

else()
  # Note: "Swift" is not added intentinally here, because it would break
  # the bootstrapping build in case no swift toolchain is installed on the host.
  project(LibSwift LANGUAGES C CXX)

  add_subdirectory(Sources)

  if(${LIBSWIFT_BUILD_MODE} STREQUAL "HOSTTOOLS")

    if (NOT CMAKE_Swift_COMPILER)
      message(FATAL_ERROR "Need a swift toolchain for building libswift")
    endif()

    add_libswift("libswift"
      SWIFT_EXEC "${SWIFT_EXEC_FOR_LIBSWIFT}")

  elseif(${LIBSWIFT_BUILD_MODE} STREQUAL "BOOTSTRAPPING" OR
         ${LIBSWIFT_BUILD_MODE} STREQUAL "BOOTSTRAPPING-WITH-HOSTLIBS")

    # Bootstrapping - stage 1, using the compiler from level 0

    add_libswift("libswift-bootstrapping1"
      SWIFT_EXEC "${CMAKE_BINARY_DIR}/bootstrapping0/bin/swiftc"
      DEPENDS "bootstrapping0-all"
      BOOTSTRAPPING 1)

    # The final build, using the compiler from stage 1

    add_libswift("libswift"
        SWIFT_EXEC "${CMAKE_BINARY_DIR}/bootstrapping1/bin/swiftc"
        DEPENDS "bootstrapping1-all")

  else()
    message(FATAL_ERROR "Unknown LIBSWIFT_BUILD_MODE '${LIBSWIFT_BUILD_MODE}'")
  endif()
endif()

