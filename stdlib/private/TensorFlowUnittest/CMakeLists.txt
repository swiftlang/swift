add_swift_library(swiftTensorFlowUnittest ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  TensorFlowUnittest.swift

  SWIFT_MODULE_DEPENDS StdlibUnittest TensorFlow
  INSTALL_IN_COMPONENT stdlib-experimental)
