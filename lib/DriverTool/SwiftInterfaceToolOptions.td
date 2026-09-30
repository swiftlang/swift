// Include the common option parsing interfaces.
include "llvm/Option/OptParser.td"

/////////
// Flags

def help : Flag<["-", "--"], "help">,
  HelpText<"Display available options">;

def action : JoinedOrSeparate<["-"], "action">,
  HelpText<"Swift Interface Tool action (minimize)">,
  MetaVarName<"<action>">;
