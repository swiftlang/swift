find_package(TensorFlow REQUIRED)

add_swift_library(swiftTensorFlowUnittest ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  TensorFlowUnittest.swift

  # TODO(b/78482298) Conditionally add "-DCUDA" when CUDA is enabled.
  SWIFT_COMPILE_FLAGS "-DCPU"
  SWIFT_MODULE_DEPENDS StdlibUnittest TensorFlow
  PRIVATE_LINK_LIBRARIES "${TF_LIBRARIES}"
  INSTALL_IN_COMPONENT stdlib-experimental)
