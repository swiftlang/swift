// Include the common option parsing interfaces.
include "llvm/Option/OptParser.td"

/////////
// Flags

def help : Flag<["-", "--"], "help">,
  HelpText<"Display available options">;

def _DASH_DASH : Option<["--"], "", KIND_REMAINING_ARGS>;

def cache_tool_action: JoinedOrSeparate<["-"], "cache-tool-action">,
  HelpText<"Swift Cache Tool Action Kind">,
  MetaVarName<"<print-base-key|print-output-keys|...>">;

def cas_path: Separate<["-"], "cas-path">,
  HelpText<"Path to CAS">, MetaVarName<"<path>">;
