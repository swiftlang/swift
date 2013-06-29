struct _CommandLineArguments {
  // This variable gets (re-)initialized on-demand to reflect the actual argument vector
  var _arguments : String[]

  var arguments : String[] {
    // FIXME: this is not thread-safe.  rdar://14193840
    if _arguments.length == 0 {
      _arguments = new String[Int(C_ARGC)]
      for i in 0..Int(C_ARGC) {
           _arguments[i] = String.fromCString(C_ARGV[i])
      }
    }
    return _arguments
  }
}

var CommandLineArguments : _CommandLineArguments
