add_custom_target(SwiftUnitTests)
set_target_properties(SwiftUnitTests PROPERTIES FOLDER "Tests")

function(add_swift_unittest test_dirname)
  add_unittest(SwiftUnitTests ${test_dirname} ${ARGN})
endfunction()

file(GLOB entries *)
foreach(entry ${entries})
  if(IS_DIRECTORY ${entry} AND EXISTS ${entry}/CMakeLists.txt)
    add_subdirectory(${entry})
  endif()
endforeach(entry)
