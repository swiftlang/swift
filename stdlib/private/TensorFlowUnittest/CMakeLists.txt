find_package(TensorFlow REQUIRED)

add_swift_library(swiftTensorFlowUnittest ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  TensorFlowUnittest.swift
  TensorFlowUnittestRemote.swift

  SWIFT_MODULE_DEPENDS Python StdlibUnittest TensorFlow
  SWIFT_MODULE_DEPENDS_OSX Darwin
  SWIFT_MODULE_DEPENDS_LINUX Glibc
  PRIVATE_LINK_LIBRARIES "${TF_LIBRARIES}"
  INSTALL_IN_COMPONENT stdlib-experimental)
