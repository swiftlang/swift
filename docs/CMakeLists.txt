find_program(SPHINX_EXECUTABLE
  NAMES sphinx-build
  HINTS $ENV{SPHINX_DIR}
  PATH_SUFFIXES bin
  DOC "Sphinx documentation generator")

SET(SWIFT_SPHINX_PAPER_SIZE "letter"
  CACHE STRING "Paper size for generated documentation")

SET(SPHINX_ARGS
  -Q
  -D latex_paper_size=${SWIFT_SPHINX_PAPER_SIZE}
  -d ${CMAKE_BINARY_DIR}/doctrees)

if(SPHINX_EXECUTABLE)
  add_custom_target(docs_html ALL
    ${SPHINX_EXECUTABLE} ${SPHINX_ARGS} -b html
      . ${CMAKE_BINARY_DIR}/docs/html
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/docs
    COMMENT "Building HTML documentation")

  # Selectively install docs intended for general consumption.
  install(
    FILES
      ${CMAKE_BINARY_DIR}/docs/html/LangRef.html
    DESTINATION share/swift/docs/html)

  install(
    DIRECTORY
      ${CMAKE_BINARY_DIR}/docs/html/_static
      ${CMAKE_BINARY_DIR}/docs/html/whitepaper
    DESTINATION share/swift/docs/html)

else(SPHINX_EXECUTABLE)
  message(WARNING "Unable to find sphinx-build program. Not building docs")
endif(SPHINX_EXECUTABLE)

## Example testing

find_program(LITRE_EXECUTABLE
  NAMES litre
  DOC "LitRe literate programming tool for docutils")

if(LITRE_EXECUTABLE)
  execute_process(
    COMMAND ${LITRE_EXECUTABLE} --cmake_module_path
    OUTPUT_VARIABLE litre_cmake_module_path
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  list(APPEND CMAKE_MODULE_PATH ${litre_cmake_module_path})

  file(GLOB_RECURSE rst_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.rst)
  foreach(rst ${rst_files})
    set(test_dir ${CMAKE_CURRENT_BINARY_DIR}/litre-tests/${rst}.litre-tests)
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E make_directory "${test_dir}"
      COMMAND ${LITRE_EXECUTABLE} --dump_dir=${test_dir} --traceback ${CMAKE_CURRENT_SOURCE_DIR}/${rst}
      )
    add_subdirectory(${test_dir} ${test_dir}/build)
  endforeach()
else()
  message(WARNING "LitRe not found; code examples won't be tested.")
endif()

