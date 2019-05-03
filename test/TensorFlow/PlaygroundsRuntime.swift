// RUN: true

// This file contains stubs for functions that the playground transform in
// lib/Sema/PlaygroundTransform.cpp generates calls into.

@inline(never)
func __builtin_log<T>(_ object : T, _ name : String, _ sl : Int, _ el : Int, _ sc : Int, _ ec: Int) -> AnyObject? {
  return nil
}

@inline(never)
func __builtin_log_with_id<T>(_ object : T, _ name : String, _ id : Int, _ sl : Int, _ el : Int, _ sc : Int, _ ec: Int) -> AnyObject? {
  return nil
}

@inline(never)
func __builtin_log_scope_entry(_ sl : Int, _ el : Int, _ sc : Int, _ ec: Int) -> AnyObject? {
  return nil
}

@inline(never)
func __builtin_log_scope_exit(_ sl : Int, _ el : Int, _ sc : Int, _ ec: Int) -> AnyObject? {
  return nil
}

@inline(never)
func __builtin_postPrint(_ sl : Int, _ el : Int, _ sc : Int, _ ec: Int) -> AnyObject? {
  return nil
}

@inline(never)
func __builtin_send_data(_ object:AnyObject?) {
}


