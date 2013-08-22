include "llvm/Option/OptParser.td"

def triple : Separate<["-"], "triple">,
  HelpText<"Target triple for IR generation">;

def import_search_path : Joined<["-"], "I">,
  HelpText<"Add a directory to the import search path">;
def framework_search_path : Joined<["-"], "F">,
  HelpText<"Add a directory to the framework search path">;

def sdk : Separate<["-"], "sdk">,
  HelpText<"Path to the SDK to build against">;

def module_cache_path : Separate<["-"], "module-cache-path">,
  HelpText<"Clang module cache path">;

def parse_stdlib : Flag<["-"], "parse-stdlib">,
  HelpText<"Parse the input as the swift standard library">;

// LangOptions
def debug_constraints : Flag<["-"], "debug-constraints">,
  HelpText<"Debug the constraint-based type checker">;
def enable_definite_init : Flag<["-"], "enable-definite-init">,
  HelpText<"Enable the 'new' model for definite initialization">;
