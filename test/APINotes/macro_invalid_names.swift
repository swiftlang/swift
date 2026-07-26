// RUN: %target-typecheck-verify-swift -verify-additional-file %/S/Inputs/custom-modules%{fs-sep}InvalidMacroNames.h -I %/S/Inputs/custom-modules

import InvalidMacroNames

_ = OPERATOR_NAMED_MACRO
_ = FUNCTION_NAMED_MACRO
_ = MEMBER_NAMED_MACRO
