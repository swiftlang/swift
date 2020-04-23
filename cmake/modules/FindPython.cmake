find_package(Python2 COMPONENTS Interpreter REQUIRED)
find_package(Python3 COMPONENTS Interpreter)
if(NOT Python3_Interpreter_FOUND)
  message(WARNING "Python3 not found, using python2 as a fallback")
  add_executable(Python3::Interpreter IMPORTED)
  set_target_properties(Python3::Interpreter PROPERTIES
  IMPORTED_LOCATION ${Python2_EXECUTABLE})
endif()
