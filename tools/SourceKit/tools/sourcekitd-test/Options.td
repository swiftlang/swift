//===--- Options.td -------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

include "llvm/Option/OptParser.td"

def req : Separate<["-"], "req">,
  HelpText<"Request kind">;
def req_EQ : Joined<["-"], "req=">, Alias<req>;

def offset : Separate<["-"], "offset">,
  HelpText<"byte offset">;
def offset_EQ : Joined<["-"], "offset=">, Alias<offset>;

def length : Separate<["-"], "length">,
  HelpText<"length of text referred to by pos / offset">;
def length_EQ : Joined<["-"], "length=">, Alias<length>;

def pos : Separate<["-"], "pos">,
  HelpText<"line:col">;
def pos_EQ : Joined<["-"], "pos=">, Alias<pos>;

def line : Separate<["-"], "line">,
  HelpText<"line">;
def line_EQ : Joined<["-"], "line=">, Alias<line>;

def replace : Separate<["-"], "replace">,
  HelpText<"replace text ('text')">;
def replace_EQ : Joined<["-"], "replace=">, Alias<replace>;

def module : Separate<["-"], "module">,
  HelpText<"Module name">;

def header : Separate<["-"], "header">,
  HelpText<"Header path">;

def usr : Separate<["-"], "usr">,
  HelpText<"USR">;

def text_input : Separate<["-"], "text-input">,
  HelpText<"File to get source text from">;

def pass_as_sourcetext : Flag<["-"], "pass-as-sourcetext">,
  HelpText<"Read source file and pass the contents with key.sourcetext">;

def cache_path: Separate<["-"], "cache-path">, HelpText<"cache path">;
def cache_path_EQ : Joined<["-"], "cache-path=">, Alias<cache_path>;

def req_opts : CommaJoined<["-"], "req-opts=">,
  HelpText<"Pass the comma separated options in <arg> as request specific options">,
  MetaVarName<"<arg>">;

def check_interface_is_ascii : Flag<["-"], "check-interface-ascii">,
  HelpText<"Check that the module interface text is ASCII">;

def json_request_path: Separate<["-"], "json-request-path">,
  HelpText<"path to read a request in JSON format">;

def dont_print_request : Flag<["-"], "dont-print-request">,
  HelpText<"Don't print the request">;

def print_response_as_json : Flag<["-"], "print-response-as-json">,
  HelpText<"Print the response as JSON output">;

def print_raw_response : Flag<["-"], "print-raw-response">,
  HelpText<"Dump the response to stdout">;

def group_name : Separate<["-"], "group-name">,
  HelpText<"Module group name to print">;

def simplified_demangling : Flag<["-"], "simplified-demangling">,
  HelpText<"Use simplified demangling for the 'demangle' request">;

def synthesized_extension : Flag<["-"], "synthesized-extension">,
  HelpText<"Print synthesized extensions when generating interface">;

def cursor_action : Flag<["-"], "cursor-action">,
  HelpText<"Print available actions on the code element under cursor">;

def interested_usr : Separate<["-"], "interested-usr">,
  HelpText<"Interested USR to calculate the containing group">;

def async : Flag<["-"], "async">,
  HelpText<"Perform request asynchronously">;

def end_pos : Separate<["-"], "end-pos">, HelpText<"line:col">;
def end_pos_EQ : Joined<["-"], "end-pos=">, Alias<end_pos>;