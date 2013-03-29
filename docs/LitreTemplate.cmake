cmake_minimum_required(VERSION 2.8)

find_program(LITRE_EXECUTABLE
  NAMES litre
  DOC "LitRe literate programming tool for docutils")

# Make the LitreTesting cmake module available
execute_process(
  COMMAND ${LITRE_EXECUTABLE} --cmake_module_path
  OUTPUT_VARIABLE litre_cmake_module_path
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )

list(APPEND CMAKE_MODULE_PATH ${litre_cmake_module_path})
set(rst_files "@rst_files@")
foreach(rst ${rst_files})
  add_subdirectory("${rst}.litre-tests")
endforeach()
