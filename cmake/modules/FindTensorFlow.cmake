# SWIFT_ENABLE_TENSORFLOW
# Find TensorFlow.

include(FindPackageHandleStandardArgs)

find_path(TF_INCLUDE_DIR
  NAMES tensorflow/c
  HINTS ${SWIFT_TENSORFLOW_TARGET_INCLUDE_DIR} /usr/include /usr/local/include)

find_library(TF_LIBRARY
  NAMES tensorflow
  HINTS ${SWIFT_TENSORFLOW_TARGET_LIB_DIR} /usr/lib /usr/local/lib)
set(TF_LIBRARIES ${TF_LIBRARY})

find_package_handle_standard_args(TensorFlow DEFAULT_MSG TF_INCLUDE_DIR TF_LIBRARIES)
mark_as_advanced(${TF_INCLUDE_DIR} ${TF_LIBRARIES})
