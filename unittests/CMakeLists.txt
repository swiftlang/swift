add_custom_target(SwiftUnitTests)

set_target_properties(SwiftUnitTests PROPERTIES FOLDER "Tests")

function(add_swift_unittest test_dirname)
  add_unittest(SwiftUnitTests ${test_dirname} ${ARGN})

  if(SWIFT_BUILT_STANDALONE AND NOT "${CMAKE_CFG_INTDIR}" STREQUAL ".")
    # Replace target references with full paths, so that we use LLVM's
    # build configuration rather than Swift's.
    get_target_property(libnames ${test_dirname} LINK_LIBRARIES)

    set(new_libnames)
    foreach(dep ${libnames})
      if("${dep}" MATCHES "^(LLVM|Clang|gtest)")
        list(APPEND new_libnames "${LLVM_LIBRARY_OUTPUT_INTDIR}/lib${dep}.a")
      else()
        list(APPEND new_libnames "${dep}")
      endif()
    endforeach()

    set_property(TARGET ${test_dirname} PROPERTY LINK_LIBRARIES ${new_libnames})
    swift_common_llvm_config(${test_dirname} support)
  endif()

  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    set_property(TARGET "${test_dirname}" APPEND_STRING PROPERTY
      LINK_FLAGS " -Xlinker -rpath -Xlinker ${SWIFT_LIBRARY_OUTPUT_INTDIR}/swift/macosx")
  endif()
endfunction()

if(SWIFT_BUILD_TOOLS)
  # We can't link C++ unit tests unless we build the tools.

  add_subdirectory(Availability)
  add_subdirectory(Basic)
  add_subdirectory(Driver)
  add_subdirectory(IDE)
  add_subdirectory(Parse)
  add_subdirectory(ReST)
  add_subdirectory(SwiftDemangle)

  if(SWIFT_BUILD_SDK_OVERLAY)
    # Runtime tests depend on symbols in StdlibUnittest.
    #
    # FIXME: cross-compile runtime unittests.
    add_subdirectory(runtime)
  endif()
endif()

