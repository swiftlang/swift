func [asmname="getenv"] posix_getenv(varname : CString) -> CString
func [asmname="setenv"] posix_setenv(varname : CString,
                                     value : CString,
                                     overwrite : CInt) -> CInt
func [asmname="unsetenv"] posix_unsetenv(varname : CString) -> CInt

struct _EnvironmentVariables {
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

var EnvironmentVariables : _EnvironmentVariables

