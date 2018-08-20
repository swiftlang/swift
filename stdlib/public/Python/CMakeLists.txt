#===--- CMakeLists.txt - Build the Python support library ----------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#
#
# SWIFT_ENABLE_TENSORFLOW
#
#===----------------------------------------------------------------------===#

include("../../../cmake/modules/FindPythonLibraries.cmake")

set(SWIFT_PYTHON_EXISTS TRUE PARENT_SCOPE)

set(SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES)
if(SWIFT_BUILD_DYNAMIC_SDK_OVERLAY)
  list(APPEND SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES SHARED)
endif()
if(SWIFT_BUILD_STATIC_SDK_OVERLAY)
  list(APPEND SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES STATIC)
endif()

foreach(python_version ${PYTHON_INTEROP_VERSIONS})
  string(SUBSTRING "${python_version}" 0 1 python_version_short)
  reset_python_variables()
  if(python_version EQUAL 2.7)
    find_package(PythonLibs "${python_version}" EXACT REQUIRED)
    set(PYTHON2_LIBRARIES ${PYTHON_LIBRARIES})
  else()
    set(Python_ADDITIONAL_VERSIONS 3.4 3.5 3.6)
    find_package(PythonLibs "${python_version}" REQUIRED)
    set(PYTHON3_LIBRARIES ${PYTHON_LIBRARIES})
  endif()
  message(STATUS "Building Python (version ${python_version}).")

  set(swift_compile_flags "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}")
  list(APPEND swift_compile_flags "-DPYTHON${python_version_short}")

  add_swift_library("swiftPython${python_version_short}" ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
    Python.swift
    NumpyConversion.swift

    DEPENDS "cpython_modulemap${python_version_short}"
    TARGET_SDKS OSX CYGWIN FREEBSD LINUX HAIKU
    SWIFT_MODULE_DEPENDS_IOS Darwin
    SWIFT_MODULE_DEPENDS_OSX Darwin
    SWIFT_MODULE_DEPENDS_TVOS Darwin
    SWIFT_MODULE_DEPENDS_WATCHOS Darwin
    SWIFT_MODULE_DEPENDS_LINUX Glibc
    SWIFT_MODULE_DEPENDS_FREEBSD Glibc
    SWIFT_MODULE_DEPENDS_CYGWIN Glibc
    SWIFT_MODULE_DEPENDS_HAIKU Glibc
    SWIFT_COMPILE_FLAGS "${swift_compile_flags}"
    PRIVATE_LINK_LIBRARIES "${PYTHON_LIBRARIES}"
    LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}")
endforeach()
