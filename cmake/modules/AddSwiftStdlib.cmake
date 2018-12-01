
# Compute the names of all swift stdlib targets that we will build.
#
# This function allows us to use the same logic to generate the list of all
# stdlib targets and also through its separating functionally of the targets in
# different lists, allows us to still generate targets as we did before.
function(
    _swift_stdlib_target_names

    # Positional input parameters
    name variant define_all_alias

    # Out params
    target_out
    all_alias_target_out
    sdk_variants_out
    sdk_arch_variants_out
    sdk_arch_variants_offset_out # This will contain a list from sdk_variant
                                 # index to the index where its elts start
    sdk_primary_variant_out
    )
  if(NOT variant STREQUAL "")
    set(variant "-${variant}")
  endif()

  set(${target_out} "${name}${variant}" PARENT_SCOPE)

  if(define_all_alias)
    set(${all_alias_target_out} "${name}${variant}-all" PARENT_SCOPE)
  endif()

  set(sdk_variants)
  set(sdk_arch_variants)
  set(sdk_arch_variants_offset)

  foreach(sdk ${SWIFT_SDKS})
    list(APPEND sdk_variants "${name}-${SWIFT_SDK_${sdk}_LIB_SUBDIR}${variant}")
    list(LENGTH sdk_arch_variants current_sdk_variants_base_offset)
    list(APPEND sdk_arch_variants_offsets ${current_sdk_variants_base_offset})
    foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
      set(target_variant -${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch})
      list(APPEND sdk_arch_variants "${name}${target_variant}${variant}")
    endforeach()
  endforeach()

  set(${sdk_variants_out} ${sdk_variants} PARENT_SCOPE)
  set(${sdk_arch_variants_out} ${sdk_arch_variants} PARENT_SCOPE)
  set(${sdk_arch_variants_offset_out} ${sdk_arch_variants_offsets} PARENT_SCOPE)
  set(${sdk_primary_variant_out} ${name}${SWIFT_PRIMARY_VARIANT_SUFFIX}${variant} PARENT)
endfunction()

function(swift_create_stdlib_targets name variant define_all_alias)
  _swift_stdlib_target_names(
    ${name}
    ${variant}
    ${define_all_aliases}
    top_level_target_name
    all_alias_target_name
    sdk_variants_name_list
    sdk_arch_variants_name_list
    sdk_arch_variants_offsets_list
    sdk_primary_variant
  )
  if(define_all_alias)
    add_custom_target(${all_alias_target_name})
    set_target_properties(${all_alias_target_name}
      PROPERTIES
      FOLDER "Swift libraries/Aggregate")
  endif()

  # Go through our list and create our targets/etc
  list(LENGTH sdk_variants_name_list sdk_variants_name_list_length)
  list(LENGTH sdk_variants_name_list_length_minus_one "${sdk_variants_name_list_length} - 1" DECIMAL)
  foreach(sdk_offset RANGE 0 ${sdk_variants_name_list_length})
    list(GET sdk_variants_name_list ${sdk_offset} sdk_variant_name)
    add_custom_target(${sdk_variant_name})
    set_target_properties(${sdk_variant_name}
      PROPERTIES
      FOLDER "Swift libraries/Aggregate")

    list(GET sdk_arch_variants_offsets_list ${sdk_offset} sdk_variant_name_list_index_start)
    if (${sdk_offset} EQUALS ${sdk_variants_name_list_length_minus_one})
      list(LENGTH sdk_arch_variants_offsets_list sdk_variant_name_list_index_end)
    else()
      math(EXPR next_sdk_offset "${sdk_offset} + 1")
      list(GET sdk_arch_variants_offsets_list "${next_sdk_offset}" sdk_variant_name_list_index_end)
    endif()

    foreach(sdk_arch_offset RANGE ${sdk_variant_name_list_index_start} ${sdk_variant_name_list_index_end})
      list(GET sdk_arch_variants_name_list ${sdk_arch_offset} sdk_arch_variants_name_list_elt)
      add_custom_target(${sdk_arch_variants_name_list_elt})
      set_target_properties(${sdk_arch_variants_name_list_elt}
        PROPERTIES
        FOLDER "Swift libraries/Aggregate")
      if(define_all_alias)
        add_dependencies(${all_alias_target_name}
          ${sdk_arch_variants_name_list_elt})
      endif()
      add_dependencies(${sdk_variant_name} ${sdk_arch_variants_name_list_elt})
    endforeach()
  endforeach()

  if(NOT define_all_alias)
    set(ALL_keyword ALL)
  endif()
  add_custom_target(${top_level_name}
    ${ALL_keyword}
    DEPENDS
    ${sdk_primary_variant}
    )
  set_target_properties(${top_level_name}
    PROPERTIES
    FOLDER "Swift libraries/Aggregate")
endfunction()
