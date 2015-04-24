class LogRecord {
  let text : String
  init(api : String, object : Any, name : String, id : Int) {
    var object_description : String = ""
    print(object, &object_description)
    text = api + "[" + name + "='" + object_description + "']"
  }
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

func $builtin_log<T>(object : T, _ name : String) -> AnyObject? {
  return LogRecord(api:"$builtin_log", object:object, name:name)
}

func $builtin_log_with_id<T>(object : T, _ name : String, _ id : Int) -> AnyObject? {
  return LogRecord(api:"$builtin_log", object:object, name:name, id:id)
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

func $builtin_send_data(object:AnyObject?, _ sl: Int, _ el: Int, _ sc: Int, _ ec: Int) {
  let loc = "[\(sl):\(sc)-\(el):\(ec)]"
  println(loc + " " + (object as! LogRecord).text)
}
