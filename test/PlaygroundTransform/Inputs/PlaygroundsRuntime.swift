class LogRecord {
  let text : String
  init(api : String, object : Any, name : String) {
    var object_description : String = ""
    print(object, &object_description)
    text = api + "[" + name + "='" + object_description + "']"
  }
  init(api : String, object: Any) {
    var object_description : String = ""
    print(object, &object_description)
    text = api + "['" + object_description + "']"
  }
  init(api: String) {
    text = api
  }
}

func $builtin_log<T>(object : T, name : String) -> AnyObject? {
  return LogRecord(api:"$builtin_log", object:object, name:name)
}

func $builtin_log_scope_entry() -> AnyObject? {
  return LogRecord(api:"$builtin_log_scope_entry")
}

func $builtin_log_scope_exit() -> AnyObject? {
  return LogRecord(api:"$builtin_log_scope_exit")
}

func $builtin_print<T>(object: T) -> AnyObject? {
  return LogRecord(api:"$builtin_print", object:object)
}

func $builtin_println<T>(object: T) -> AnyObject? {
  return LogRecord(api:"$builtin_println", object:object)
}

func $builtin_send_data(object:AnyObject?, sl: Int, el: Int, sc: Int, ec: Int) {
  let loc : String = "[" + String(sl) + ":" + String(sc)
                   + "-" + String(el) + ":" + String(ec) + "]"
  println(loc + " " + (object as LogRecord).text)
}
