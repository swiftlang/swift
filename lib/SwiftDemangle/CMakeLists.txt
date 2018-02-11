add_swift_library(swiftDemangle SHARED
  SwiftDemangle.cpp
  MangleHack.cpp
  LINK_LIBRARIES swiftDemangling)

swift_install_in_component(compiler
    TARGETS swiftDemangle
    LIBRARY DESTINATION "${SWIFT_LIBDIR}"
    ARCHIVE DESTINATION "${SWIFT_LIBDIR}")
