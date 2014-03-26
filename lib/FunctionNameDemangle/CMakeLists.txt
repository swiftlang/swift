add_swift_library(functionNameDemangle SHARED
  FunctionNameDemangle.cpp
  DEPENDS swiftBasic
  COMPONENT_DEPENDS support)

install(TARGETS functionNameDemangle
      LIBRARY DESTINATION lib${LLVM_LIBDIR_SUFFIX}
      ARCHIVE DESTINATION lib${LLVM_LIBDIR_SUFFIX})
