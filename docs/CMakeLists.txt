find_program(SPHINX_EXECUTABLE
  NAMES sphinx-build
  HINTS $ENV{SPHINX_DIR}
  PATH_SUFFIXES bin
  DOC "Sphinx documentation generator")

SET(SWIFT_SPHINX_PAPER_SIZE "letter"
  CACHE STRING "Paper size for generated documentation")

SET(SPHINX_ARGS
  -q
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
