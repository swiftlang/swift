func [asmname="getenv"] posix_getenv(varname : CString) -> CString
func [asmname="setenv"] posix_setenv(varname : CString,
                                     value : CString,
                                     overwrite : CInt) -> CInt
func [asmname="unsetenv"] posix_unsetenv(varname : CString) -> CInt

struct _Process {
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

  struct EnvironmentVariables {

    func find(varName : String) -> (Bool, String) {
      var LM = LifetimeManager()
      var found = posix_getenv(LM.getCString(varName))
      LM.release()
      if (found.isNull()) {
        return (false, "")
      } else {
        return (true, String.fromCString(found))
      }
    }

    subscript(varName : String) -> String {
    get:
      return find(varName).1
    set(value):
      var LM = LifetimeManager()
      var result = posix_setenv(LM.getCString(varName),
                                LM.getCString(value),
                                1)
      LM.release()
      assert(result == 0)
    }

    func erase(varName : String) {
      var LM = LifetimeManager()
      var result = posix_unsetenv(LM.getCString(varName))
      LM.release()
      assert(result == 0)
    }
  }

  var environmentVariables : EnvironmentVariables {
    var result : EnvironmentVariables
    return result
  }
}

var Process : _Process
