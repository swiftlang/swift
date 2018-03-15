
function(initialize_local_jobpool)
  # Make a job pool for things that can't yet be distributed
  cmake_host_system_information(
    RESULT localhost_logical_cores QUERY NUMBER_OF_LOGICAL_CORES)
  set_property(GLOBAL PROPERTY JOB_POOLS local_jobs=${localhost_logical_cores})
  # Put linking in that category.
  set_property(GLOBAL PROPERTY JOB_POOL_LINK local_jobs)
endfunction()

function(add_target_to_local_jobpool target)
  set_property(TARGET ${target} PROPERTY JOB_POOL_COMPILE local_jobs)
  # We do not need to set link jobs to be in local_jobs since above, we make
  # linking always local.
endfunction()
