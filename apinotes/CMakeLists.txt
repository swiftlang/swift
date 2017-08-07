set(SWIFT_API_NOTES_INPUTS
  Accelerate
  Dispatch
  iAd
  ModelIO
  ScreenSaver
  ScriptingBridge
  os
)

if(NOT DEFINED SWIFT_API_NOTES_PATH)
  message(FATAL_ERROR "Define SWIFT_API_NOTES_PATH before including this file")
endif()

foreach(module ${SWIFT_API_NOTES_INPUTS})
  if(NOT EXISTS "${SWIFT_API_NOTES_PATH}/${module}.apinotes")
    message(SEND_ERROR "Missing apinotes for ${module}")
  endif()
endforeach()

file(GLOB SWIFT_API_NOTES_INPUT_FILES "${SWIFT_API_NOTES_PATH}/*.apinotes")
foreach(file ${SWIFT_API_NOTES_INPUT_FILES})
  get_filename_component(name "${file}" NAME_WE)
  if(NOT "${name}" IN_LIST SWIFT_API_NOTES_INPUTS)
    message(SEND_ERROR "Found apinotes for ${name}; please add to CMakeLists.txt")
  endif()
endforeach()
