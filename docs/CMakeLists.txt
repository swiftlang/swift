find_program(SPHINX_EXECUTABLE
  NAMES sphinx-build
  HINTS $ENV{SPHINX_DIR}
  PATH_SUFFIXES bin
  DOC "Sphinx documentation generator")

SET(SWIFT_SPHINX_PAPER_SIZE "letter"
  CACHE STRING "Paper size for generated documentation")

SET(SPHINX_ARGS
  -D latex_paper_size=${SWIFT_SPHINX_PAPER_SIZE}
  -d ${CMAKE_BINARY_DIR}/doctrees)

if(SPHINX_EXECUTABLE)
  add_custom_target(docs_html ALL
    ${SPHINX_EXECUTABLE} ${SPHINX_ARGS} -b html
      . ${CMAKE_BINARY_DIR}/docs/html
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
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

find_program(SPHINX_EXECUTABLE
  NAMES sphinx-build
  HINTS $ENV{SPHINX_DIR}
  PATH_SUFFIXES bin
  DOC "Sphinx documentation generator")

if(LITRE_EXECUTABLE)
  # Find all the .rst files
  file(GLOB_RECURSE rst_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.rst)
  set(subdir_CMakeLists)

  foreach(rst ${rst_files})
    # Prepare a testing directory containing a CMakeLists.txt 
    # and example files extracted from the .rst
    set(test_dir "litre-tests/${rst}.litre-tests")

    add_custom_command(
      OUTPUT 
        ${test_dir}/CMakeLists.txt
      COMMAND 
        ${LITRE_EXECUTABLE} 
        --default_compiler=${CMAKE_BINARY_DIR}/bin/swift
        "--dump_dir=${test_dir}"
        --traceback 
        --report=severe  # suppress most .rst errors.  We have lots of them.
        ${CMAKE_CURRENT_SOURCE_DIR}/${rst}
      DEPENDS
        ${CMAKE_CURRENT_SOURCE_DIR}/${rst}
      COMMENT
        "Generating/Updating LitRe tests for ${rst}"
      VERBATIM
      )

    list(APPEND subdir_CMakeLists ${test_dir}/CMakeLists.txt)
  endforeach()

  # Create a top-level CMakeLists.txt in a temporary file
  add_custom_command(
    OUTPUT
      litre-top-CMakeLists.cmake
    COMMAND
      ${CMAKE_COMMAND} -DOUTPUT=litre-top-CMakeLists.cmake 
                      -DSOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}
          -P ${CMAKE_CURRENT_SOURCE_DIR}/GenerateTopLevelLitreCMakeLists.cmake
          ${rst_files}
    DEPENDS
      ${rst_files}
    COMMENT
      "Generating top-level LitRe CMakeLists.txt content"
    VERBATIM
    )

  # Only update the real top-level CMakeLists.txt if something  changed
  add_custom_command(
    OUTPUT
      litre-tests/CMakeLists.txt
    COMMAND
      ${CMAKE_COMMAND} -E copy_if_different litre-top-CMakeLists.cmake litre-tests/CMakeLists.txt
    DEPENDS
      litre-top-CMakeLists.cmake
    COMMENT
      "Updating top-level LitRe CMakeLists.txt"
    VERBATIM
      )

  # Create a build directory
  add_custom_command(
    OUTPUT litre-tests/build 
    COMMAND ${CMAKE_COMMAND} -E make_directory litre-tests/build
    )

  # Run CMake itself to configure/build the tests
  add_custom_command(
    OUTPUT
      litre-tests/results
    COMMAND
      ${CMAKE_COMMAND} -G "${CMAKE_GENERATOR}" "${CMAKE_CURRENT_BINARY_DIR}/litre-tests"
    COMMAND
      ${CMAKE_COMMAND} --build .
    COMMAND
      ${CMAKE_COMMAND} -E touch "${CMAKE_CURRENT_BINARY_DIR}/litre-tests/results"
    WORKING_DIRECTORY
      litre-tests/build
    DEPENDS
      ${CMAKE_BINARY_DIR}/bin/swift
      litre-tests/CMakeLists.txt litre-tests/build ${subdir_CMakeLists}
    COMMENT
      "Running LitRe tests"
    VERBATIM
      )

  # Add a target so these tests show up in the Xcode project.
  add_custom_target(
    LiterateTests SOURCES ${rst_files}
    DEPENDS litre-tests/results
    )
  set_target_properties(LiterateTests PROPERTIES FOLDER "Tests")
else()
  message(WARNING "LitRe not found; code examples won't be tested.")
endif()

