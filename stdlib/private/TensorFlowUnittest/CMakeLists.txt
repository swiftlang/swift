find_package(TensorFlow REQUIRED)

add_swift_target_library(swiftTensorFlowUnittest ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  # This file should be listed first. Module name is inferred from the filename.
  TensorFlowUnittest.swift
  TensorFlowUnittestRemote.swift

  SWIFT_MODULE_DEPENDS Python StdlibUnittest TensorFlow
  SWIFT_MODULE_DEPENDS_OSX Darwin
  SWIFT_MODULE_DEPENDS_LINUX Glibc
  PRIVATE_LINK_LIBRARIES "${TF_LIBRARIES}"
  INSTALL_IN_COMPONENT stdlib-experimental
  DARWIN_INSTALL_NAME_DIR "${SWIFT_DARWIN_STDLIB_PRIVATE_INSTALL_NAME_DIR}")
