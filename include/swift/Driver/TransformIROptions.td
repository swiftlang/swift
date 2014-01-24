//===--- TransformIROptions.td - Options for swift -transform-ir ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines the options accepted by swift -transform-ir.
//
//===----------------------------------------------------------------------===//

def transform_ir_modes_Group : OptionGroup<"<transform IR mode options>">;
class TransformIRModeOpt : Group<transform_ir_modes_Group>;

let Flags = [TransformIROption, NoDriverOption] in {

def arc_optimize : Flag<["-"], "arc-optimize">, HelpText<"">,
  TransformIRModeOpt;

def arc_expand : Flag<["-"], "arc-expand">, HelpText<"">,
  TransformIRModeOpt;

} // end let Flags = [TransformIROption, NoDriverOption]
