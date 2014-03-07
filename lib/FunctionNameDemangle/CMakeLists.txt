add_swift_library(functionNameDemangle SHARED INSTALL
  FunctionNameDemangle.cpp
  DEPENDS swiftBasic
  COMPONENT_DEPENDS support)
