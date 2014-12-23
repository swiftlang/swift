add_swift_library(functionNameDemangle SHARED
  FunctionNameDemangle.cpp
  MangleHack.cpp
  LINK_LIBRARIES swiftBasic
  COMPONENT_DEPENDS support)

swift_install_in_component(compiler
    TARGETS functionNameDemangle
    LIBRARY DESTINATION "lib${LLVM_LIBDIR_SUFFIX}"
    ARCHIVE DESTINATION "lib${LLVM_LIBDIR_SUFFIX}")

