// RUN: %target-build-swift -emit-module -emit-library %S/Inputs/public_var_private_setter.swift
// RUN: %target-build-swift -I . -L . -lpublic_var_private_setter %s -o use_public_var_private_setter

// On Linux, the linker step of this test fails with "Bad value", specifically:
// "hidden symbol `_TFC25public_var_private_setter9BaseClasscfT_S0_' isn't defined".
// XFAIL: linux

import public_var_private_setter

class Class: BaseClass {}
