// Include the common option parsing interfaces.
include "llvm/Option/OptParser.td"

/////////
// Flags

def help : Flag<["-", "--"], "help">,
  HelpText<"Display available options">;

def action : JoinedOrSeparate<["-"], "action">,
  HelpText<"Swift Interface Tool action">,
  MetaVarName<"<minimize-source|extract-imports|extract-module-imports>">;

def internal_import_by_default : Flag<["-"], "internal-import-by-default">,
  HelpText<"Treat bare imports without an explicit access modifier as internal "
           "(removed during minimization) instead of public (kept)">;

def remove_internal_decls : Flag<["-"], "remove-internal-decls">,
  HelpText<"Remove internal and fileprivate declarations during minimization "
           "(by default only function bodies are stripped)">;
