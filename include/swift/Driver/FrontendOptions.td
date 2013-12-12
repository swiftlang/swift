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

def emit_verbose_sil : Flag<["-"], "emit-verbose-sil">,
  Flags<[HelpHidden]>, HelpText<"Emit locations during SIL emission">;

} // end let Flags = [FrontendOption, NoDriverOption]
