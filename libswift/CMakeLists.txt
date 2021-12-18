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

  if(${LIBSWIFT_BUILD_MODE} MATCHES "HOSTTOOLS|CROSSCOMPILE-WITH-HOSTLIBS")

    if (NOT SWIFT_EXEC_FOR_LIBSWIFT)
      message(FATAL_ERROR "Need a swift toolchain for building libswift")
    endif()

    add_libswift("libswift"
      SWIFT_EXEC "${SWIFT_EXEC_FOR_LIBSWIFT}")

  elseif(${LIBSWIFT_BUILD_MODE} MATCHES "BOOTSTRAPPING.*")

    set(b0_deps swift-frontend-bootstrapping0 symlink-headers-bootstrapping0)
    set(b1_deps swift-frontend-bootstrapping1 symlink-headers-bootstrapping1)
    if(${LIBSWIFT_BUILD_MODE} STREQUAL "BOOTSTRAPPING")
      list(APPEND b0_deps swiftCore-bootstrapping0)
      list(APPEND b1_deps swiftCore-bootstrapping1)
      if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        list(APPEND b0_deps swiftSwiftOnoneSupport-bootstrapping0)
        list(APPEND b1_deps swiftSwiftOnoneSupport-bootstrapping1)
      endif()
      if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_DARWIN_PLATFORMS)
        list(APPEND b0_deps swiftDarwin-bootstrapping0)
        list(APPEND b1_deps swiftDarwin-bootstrapping1)
      endif()
    endif()
    if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_DARWIN_PLATFORMS)
      set(platform ${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR})
      set(compatibility_libs
          "swiftCompatibility50-${platform}"
          "swiftCompatibility51-${platform}"
          "swiftCompatibilityDynamicReplacements-${platform}")

      list(APPEND b0_deps ${compatibility_libs})
      list(APPEND b1_deps ${compatibility_libs})
    endif()


    # Bootstrapping - stage 1, using the compiler from level 0

    add_libswift(libswift-bootstrapping1
      SWIFT_EXEC "${CMAKE_BINARY_DIR}/bootstrapping0/bin/swiftc"
      DEPENDS ${b0_deps}
      BOOTSTRAPPING 1)

    # The final build, using the compiler from stage 1

    add_libswift(libswift
        SWIFT_EXEC "${CMAKE_BINARY_DIR}/bootstrapping1/bin/swiftc"
        DEPENDS ${b1_deps})

    if(LIBSWIFT_BUILD_MODE STREQUAL "BOOTSTRAPPING-WITH-HOSTLIBS")
      file(GLOB module_dirs "${CMAKE_BINARY_DIR}/bootstrapping*/lib/swift/macosx/*.swiftmodule")
      foreach(module_dir ${module_dirs})
        message(WARNING "${module_dir} found from a previous 'bootstrapping' build: removing")
        file(REMOVE_RECURSE "${module_dir}")
      endforeach()
    endif()
  else()
    message(FATAL_ERROR "Unknown LIBSWIFT_BUILD_MODE '${LIBSWIFT_BUILD_MODE}'")
  endif()
endif()

