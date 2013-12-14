//===--- FrontendOptions.td - Options for swift -frontend -----------------===//
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
//  This file defines the options accepted by swift -frontend.
//
//===----------------------------------------------------------------------===//

let Flags = [FrontendOption, NoDriverOption] in {

def debug_constraints : Flag<["-"], "debug-constraints">,
  Flags<[HelpHidden]>, HelpText<"Debug the constraint-based type checker">;

def debug_constraints_attempt : Separate<["-"], "debug-constraints-attempt">,
  Flags<[HelpHidden]>,
  HelpText<"Debug the constraint solver at a given attempt">;
def debug_constraints_attempt_EQ : Joined<["-"], "debug-constraints-attempt=">,
  Flags<[HelpHidden]>, Alias<debug_constraints_attempt>;

def delayed_function_body_parsing :
  Flag<["-"], "delayed-function-body-parsing">,
  HelpText<"Delay function body parsing until the end of all files">;

def emit_sil_protocol_witness_tables :
  Flag<["-"], "emit-sil-protocol-witness-tables">, Flags<[HelpHidden]>,
  HelpText<"Emit experimental SIL protocol witness tables">;

def emit_verbose_sil : Flag<["-"], "emit-verbose-sil">,
  Flags<[HelpHidden]>, HelpText<"Emit locations during SIL emission">;

def print_stats : Flag<["-"], "print-stats">,
  Flags<[HelpHidden]>, HelpText<"Print various statistics">;

def use_malloc : Flag<["-"], "use-malloc">,
  Flags<[HelpHidden]>,
  HelpText<"Allocate internal data structures using malloc "
           "(for memory debugging)">;

def verify : Flag<["-"], "verify">,
  HelpText<"Verify diagnostics against expected-{error|warning|note} "
           "annotations">;

} // end let Flags = [FrontendOption, NoDriverOption]
