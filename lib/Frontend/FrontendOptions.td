include "llvm/Option/OptParser.td"

def triple : Separate<["-"], "triple">,
  HelpText<"Target triple for IR generation">;

def import_search_path : Separate<["-"], "I">,
  HelpText<"Add a directory to the import search path">;
def framework_search_path : Separate<["-"], "F">,
  HelpText<"Add a directory to the framework search path">;

def module_name : Separate<["-"], "module-name">,
  HelpText<"Name of the module to build">;

def sdk : Separate<["-"], "sdk">,
  HelpText<"Path to the SDK to build against">;

def module_cache_path : Separate<["-"], "module-cache-path">,
  HelpText<"Clang module cache path">;

def output : Separate<["-"], "o">,
  HelpText<"Output filename">;

def serialized_diagnostics_path : Separate<["-"], "serialize-diagnostics">,
  HelpText<"File for serializing diagnostics in a binary format">;

def module_source_list_path : Separate<["-"], "module-source-list">,
  HelpText<"Provide a list of all source files in the module">;

def parse_as_library : Flag<["-"], "parse-as-library">,
  HelpText<"Declare that the input is a library, not a script">;

def parse_stdlib : Flag<["-"], "parse-stdlib">,
  HelpText<"Parse the input as the swift standard library">;

def link_library : Joined<["-"], "l">,
  HelpText<"Link the given library into the output product">;

def Xclang : Separate<["-"], "Xclang">,
  HelpText<"Pass <arg> to the clang importer">;

// LangOptions
def debug_constraints : Flag<["-"], "debug-constraints">,
  HelpText<"Debug the constraint-based type checker">;
