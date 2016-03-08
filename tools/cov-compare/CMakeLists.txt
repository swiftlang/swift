add_swift_executable(cov-compare
  ProfdataCompare.cpp
  HTMLWriter.cpp
  MarkdownWriter.cpp
  YAMLWriter.cpp
  Utils.cpp
  main.cpp
  COMPONENT_DEPENDS support object profiledata "option" core)

swift_install_in_component(compiler
    TARGETS cov-compare
    RUNTIME DESTINATION "bin")

