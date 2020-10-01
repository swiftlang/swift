
include(AddSwift)

add_custom_target(SwiftUnitTests)

set_target_properties(SwiftUnitTests PROPERTIES FOLDER "Tests")

function(add_swift_unittest test_dirname)
  # *NOTE* Even though "add_unittest" does not have llvm in its name, it is a
  # function defined by AddLLVM.cmake.
  add_unittest(SwiftUnitTests ${test_dirname} ${ARGN})

  # TODO: _add_variant_c_compile_link_flags and these tests should share some
  # sort of logic.
  #
  # *NOTE* The unittests are never built for the target, so we always enable LTO
  # *if we are asked to.
  _compute_lto_flag("${SWIFT_TOOLS_ENABLE_LTO}" _lto_flag_out)
  if (_lto_flag_out)
    set_property(TARGET "${test_dirname}" APPEND_STRING PROPERTY COMPILE_FLAGS " ${_lto_flag_out} ")
    set_property(TARGET "${test_dirname}" APPEND_STRING PROPERTY LINK_FLAGS " ${_lto_flag_out} ")
  endif()

  if(SWIFT_BUILT_STANDALONE AND NOT "${CMAKE_CFG_INTDIR}" STREQUAL ".")
    # Replace target references with full paths, so that we use LLVM's
    # build configuration rather than Swift's.
    get_target_property(libnames ${test_dirname} LINK_LIBRARIES)

    set(new_libnames)
    foreach(dep ${libnames})
      if("${dep}" MATCHES "^(LLVM|Clang|gtest)" AND NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
        list(APPEND new_libnames "${LLVM_LIBRARY_OUTPUT_INTDIR}/lib${dep}.a")
      else()
        list(APPEND new_libnames "${dep}")
      endif()
    endforeach()

    set_property(TARGET ${test_dirname} PROPERTY LINK_LIBRARIES ${new_libnames})
    swift_common_llvm_config(${test_dirname} support)
  endif()

  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    set_target_properties(${test_dirname} PROPERTIES
      BUILD_RPATH ${SWIFT_LIBRARY_OUTPUT_INTDIR}/swift/macosx)
  elseif("${SWIFT_HOST_VARIANT}" STREQUAL "android")
    swift_android_lib_for_arch(${SWIFT_HOST_VARIANT_ARCH} android_system_libs)
    set_property(TARGET "${test_dirname}" APPEND PROPERTY LINK_DIRECTORIES
      "${android_system_libs}")
    set_property(TARGET "${test_dirname}" APPEND PROPERTY LINK_LIBRARIES "log")
  elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
    if(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64|AMD64")
      target_compile_options(${test_dirname} PRIVATE
        -march=core2)
    endif()
  elseif("${SWIFT_HOST_VARIANT}" STREQUAL "windows")
    target_compile_definitions("${test_dirname}" PRIVATE
      _ENABLE_EXTENDED_ALIGNED_STORAGE)
  endif()

  if(NOT SWIFT_COMPILER_IS_MSVC_LIKE)
    if(SWIFT_USE_LINKER)
      target_link_options(${test_dirname} PRIVATE
        -fuse-ld=${SWIFT_USE_LINKER}$<$<STREQUAL:${CMAKE_HOST_SYSTEM_NAME},Windows>:.exe>)
    endif()
  endif()

  if(SWIFT_ANALYZE_CODE_COVERAGE)
    set_property(TARGET "${test_dirname}" APPEND_STRING PROPERTY
      LINK_FLAGS " -fprofile-instr-generate -fcoverage-mapping")
  endif()

  if(SWIFT_RUNTIME_USE_SANITIZERS)
    if("Thread" IN_LIST SWIFT_RUNTIME_USE_SANITIZERS)
      set_property(TARGET "${test_dirname}" APPEND_STRING PROPERTY COMPILE_FLAGS
        " -fsanitize=thread")
      set_property(TARGET "${test_dirname}" APPEND_STRING PROPERTY
        LINK_FLAGS " -fsanitize=thread")
    endif()
  endif()
endfunction()


