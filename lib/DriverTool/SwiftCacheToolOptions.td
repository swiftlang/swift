// Include the common option parsing interfaces.
include "llvm/Option/OptParser.td"

/////////
// Subcommands

def sc_print_base_key : SubCommand<"print-base-key",
  "Print the base cache key of a swift-frontend invocation">;
def sc_print_output_keys : SubCommand<"print-output-keys",
  "Print the output cache keys of a swift-frontend invocation">;
def sc_validate_outputs : SubCommand<"validate-outputs",
  "Validate that the outputs of cache keys are in the CAS">;
def sc_render_diags : SubCommand<"render-diags",
  "Render the cached diagnostics of cache keys">;
def sc_print_include_tree_list : SubCommand<"print-include-tree-list",
  "Print the file list of an include tree">;
def sc_print_compile_cache_key : SubCommand<"print-compile-cache-key",
  "Print the contents of a compile job cache key">;

defvar sc_all = [sc_print_base_key, sc_print_output_keys, sc_validate_outputs,
                 sc_render_diags, sc_print_include_tree_list,
                 sc_print_compile_cache_key];

/////////
// Flags

def help : Flag<["-", "--"], "help">,
  HelpText<"Display available options">;

def _DASH_DASH : Option<["--"], "", KIND_REMAINING_ARGS,
  [sc_print_base_key, sc_print_output_keys, sc_render_diags]>;

// The legacy spelling of the subcommand, e.g.
// `-cache-tool-action print-base-key`.
def cache_tool_action: JoinedOrSeparate<["-"], "cache-tool-action">,
  HelpText<"Swift Cache Tool Action Kind">,
  MetaVarName<"<print-base-key|print-output-keys|...>">,
  Flags<[HelpHidden]>;

def cas_path: Separate<["-"], "cas-path", sc_all>,
  HelpText<"Path to CAS">, MetaVarName<"<path>">;

def cas_plugin_path: Separate<["-"], "cas-plugin-path", sc_all>,
  HelpText<"Path to CAS Plugin">, MetaVarName<"<path>">;

def cas_plugin_option: Separate<["-"], "cas-plugin-option", sc_all>,
  HelpText<"Option pass to CAS Plugin">, MetaVarName<"<name>=<option>">;
