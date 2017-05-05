#===--- AddSwiftRuntime.cmake --------------------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https:#swift.org/LICENSE.txt for license information
# See https:#swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

include(SwiftUtils)

function(swift_runtime_enable_backtrace_reporting outvar)
  precondition(outvar "Must have an outvar set")

  # We do not support backtraces on android...
  is_sdk_requested(ANDROID swift_build_android)
  if(${swift_build_android})
    set(${outvar} FALSE PARENT_SCOPE)
    return()
  endif()

  # ... or cygwin
  if ("${CMAKE_SYSTEM_NAME}" STREQUAL "CYGWIN")
    set(${outvar} FALSE PARENT_SCOPE)
    return()
  endif()

  # ... or win32.
  if (WIN32)
    set(${outvar} FALSE PARENT_SCOPE)
    return()
  endif()

  # If we are not Darwin, but are cygwin, win32, or android we *do* support
  # runtime backtraces. This is just preserving the current existing
  # behavior. Arguably this should be an opt in feature always.
  if (NOT "${CMAKE_SYSTEM_NAME}" STREQUAL Darwin)
    set(${outvar} TRUE PARENT_SCOPE)
    return()
  endif()

  # Otherwise, we have a Darwin build. Enable runtime backtrace reporting only
  # when compiler assertions are enabled.
  if ("${SWIFT_STDLIB_ASSERTIONS}")
    set(${outvar} TRUE PARENT_SCOPE)
  else()
    set(${outvar} FALSE PARENT_SCOPE)
  endif()
endfunction()
