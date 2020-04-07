#!/bin/sh
${PYTHON_EXECUTABLE} ${LIT} \
  ${args} \
  ${test_bin_dir}/ \
  --filter=${component_name}
