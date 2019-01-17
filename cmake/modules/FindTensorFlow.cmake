# SWIFT_ENABLE_TENSORFLOW
# Find TensorFlow.

include(FindPackageHandleStandardArgs)

find_path(TF_INCLUDE_DIR
  NAMES third_party/tensorflow/c tensorflow/c
  HINTS ${SWIFT_TENSORFLOW_TARGET_INCLUDE_DIR} /usr/include /usr/local/include)
if (EXISTS ${TF_INCLUDE_DIR}/third_party/tensorflow/c/c_api.h)
  # This is experimental and not covered by CI.
  set(TF_PATH_ADJUSTMENT "third_party")
else()
  # Note: This is the normal workflow.
  set(TF_PATH_ADJUSTMENT "")
endif()

find_library(TF_LIBRARY
  NAMES tensorflow
  HINTS ${SWIFT_TENSORFLOW_TARGET_LIB_DIR} /usr/lib /usr/local/lib)
find_library(TF_FRAMEWORK_LIBRARY
  NAMES tensorflow_framework
  HINTS ${SWIFT_TENSORFLOW_TARGET_LIB_DIR} /usr/lib /usr/local/lib)
set(TF_LIBRARIES ${TF_LIBRARY} ${TF_FRAMEWORK_LIBRARY})

find_package_handle_standard_args(TensorFlow DEFAULT_MSG TF_INCLUDE_DIR TF_LIBRARIES)
mark_as_advanced(${TF_INCLUDE_DIR} ${TF_LIBRARIES})
