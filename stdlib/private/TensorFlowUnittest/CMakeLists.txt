find_package(TensorFlow REQUIRED)

set(swift_tensorflow_unittest_compile_flags "-DCPU")
if(SWIFT_ENABLE_TENSORFLOW_GPU)
  message(STATUS "WOWOWOWOWOWOWOWOWOWWOWOW")
  list(APPEND swift_tensorflow_unittest_compile_flags "-DCUDA")
endif()

add_swift_library(swiftTensorFlowUnittest ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  TensorFlowUnittest.swift

  SWIFT_COMPILE_FLAGS "${swift_tensorflow_unittest_compile_flags}"
  SWIFT_MODULE_DEPENDS StdlibUnittest TensorFlow
  PRIVATE_LINK_LIBRARIES "${TF_LIBRARIES}"
  INSTALL_IN_COMPONENT stdlib-experimental)
