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

  set(bs1_libs)
  set(final_libs)
  if(${LIBSWIFT_BUILD_MODE} MATCHES "BOOTSTRAPPING|CROSSCOMPILE")
    set(bs1_libs ${bs1_libs} swiftCore-bootstrapping0-static)
    set(final_libs ${final_libs} swiftCore-bootstrapping1-static)
    set_property(GLOBAL APPEND PROPERTY SWIFT_BUILDTREE_EXPORTS swiftCore-bootstrapping1-static)
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
      set(bs1_libs ${bs1_libs} swiftOnoneSupport-bootstrapping0-static)
      set(final_libs ${final_libs} swiftOnoneSupport-bootstrapping1-static)
      set_property(GLOBAL APPEND PROPERTY SWIFT_BUILDTREE_EXPORTS swiftSwiftOnoneSupport-bootstrapping1-static)
    endif()
    if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_DARWIN_PLATFORMS)
      set(bs1_libs ${bs1_libs} swiftDarwin-bootstrapping0-static)
      set(final_libs ${final_libs} swiftDarwin-bootstrapping1-static)
      set_property(GLOBAL APPEND PROPERTY SWIFT_BUILDTREE_EXPORTS swiftDarwin-bootstrapping1-static)
    endif()
  endif()


  if(${LIBSWIFT_BUILD_MODE} MATCHES "HOSTTOOLS|CROSSCOMPILE.*")

    if (NOT SWIFT_EXEC_FOR_LIBSWIFT)
      message(FATAL_ERROR "Need a swift toolchain for building libswift")
    endif()
 
    add_libswift("libswift"
      SWIFT_EXEC "${SWIFT_EXEC_FOR_LIBSWIFT}"
      DEPENDS ${final_libs})

    target_link_libraries(libswift PUBLIC ${final_libs})

  elseif(${LIBSWIFT_BUILD_MODE} MATCHES "BOOTSTRAPPING.*")

    # Bootstrapping - stage 1, using the compiler from level 0

    add_libswift("libswift-bootstrapping1"
      SWIFT_EXEC "${CMAKE_BINARY_DIR}/bootstrapping0/bin/swiftc"
      DEPENDS swift-frontend-bootstrapping0 symlink-headers-bootstrapping0 ${bs1_libs}
      BOOTSTRAPPING 1)

    # The final build, using the compiler from stage 1

    add_libswift("libswift"
        SWIFT_EXEC "${CMAKE_BINARY_DIR}/bootstrapping1/bin/swiftc"
        DEPENDS swift-frontend-bootstrapping0 symlink-headers-bootstrapping0 ${final_libs})

    target_link_libraries(libswift-bootstrapping1 PUBLIC ${bs1_libs})
    target_link_libraries(libswift PUBLIC ${final_libs})

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

  set_property(GLOBAL APPEND PROPERTY SWIFT_BUILDTREE_EXPORTS libswift)
endif()

