add_custom_target(SwiftUnitTests)

set_target_properties(SwiftUnitTests PROPERTIES FOLDER "Tests")

function(add_swift_unittest test_dirname)
  add_unittest(SwiftUnitTests ${test_dirname} ${ARGN})

  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    set_property(TARGET "${test_dirname}" APPEND_STRING PROPERTY
      LINK_FLAGS " -Xlinker -rpath -Xlinker ${SWIFT_LIBRARY_OUTPUT_INTDIR}/swift/macosx")
  endif()
endfunction()

# FIXME: cross-compile runtime unittests.
file(GLOB entries *)
foreach(entry ${entries})
  if(IS_DIRECTORY ${entry} AND EXISTS ${entry}/CMakeLists.txt)
    add_subdirectory(${entry})
  endif()
endforeach()

