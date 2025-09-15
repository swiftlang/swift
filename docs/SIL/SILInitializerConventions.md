# SIL Initializer Conventions

A nominal type can define a number of initializers, some of which may
delegate initialization to another initializer. There are specific calling
conventions for these initializers within SIL that make up a part of the ABI
for a type. This document aims to summarize the key calling conventions for
these initializers.


# Structs and Enums

The delegation status for the initializer of a struct or enum is not encoded
in the definitions of these initializers. Thus, all of these initializers
have an implicit `metatype` argument for the instance to be passed in as the
last argument to the initializer. Using `<...>` as a stand-in for other
arguments that are part of the usual function calling convention, consider this
example:

```swift
// the non-delegating init MyStruct.init(final:)
sil hidden [ossa] @$s4test8MyStructV5finalACSi_tcfC : $@convention(method) (<...>, @thin MyStruct.Type) -> MyStruct {
bb0(<...>, %meta : $@thin MyStruct.Type):
  %a = alloc_box ${ var MyStruct }, var, name "self"
  %b = mark_uninitialized [rootself] %a : ${ var MyStruct }
  %c = begin_borrow [lexical] %b : ${ var MyStruct }
  %d = project_box %c : ${ var MyStruct }, 0
  
  // ... initialize properties, etc ...
  
  %end = load [trivial] %d : $*MyStruct
  end_borrow %c : ${ var MyStruct }
  destroy_value %b : ${ var MyStruct }
  return %end : $MyStruct
}


// the delegating init MyStruct.init(delegates:)
sil hidden [ossa] @$s4test8MyStructV9delegatesACyt_tcfC : $@convention(method) (<...>, @thin MyStruct.Type) -> MyStruct {
bb0(<...>, %meta : $@thin MyStruct.Type):
  // Same allocation as the non-delegating:
  %a = alloc_box ${ var MyStruct }, var, name "self"
  %b = mark_uninitialized [rootself] %a : ${ var MyStruct }
  %c = begin_borrow [lexical] %b : ${ var MyStruct }
  %d = project_box %c : ${ var MyStruct }, 0
  
  // ... delegate to MyStruct.init(final:) ...
  
  %ctor = function_ref @$s4test8MyStructV5finalACSi_tcfC : $@convention(method) (Int, @thin MyStruct.Type) -> MyStruct
  %ret = apply %ctor(<...>, %meta) : $@convention(method) (Int, @thin MyStruct.Type) -> MyStruct
  
  assign %ret to %d : $*MyStruct
  %end = load [trivial] %d : $*MyStruct
  end_borrow %c : ${ var MyStruct }
  destroy_value %b : ${ var MyStruct }
  return %end : $MyStruct
}
```

It's important to note that all initializers take a metadata argument, 
regardless of whether it is a delegating initializer. There is also no
separation between allocating and non-allocating initializer entrypoints.
All initializers may perform allocation.

# Classes

Every designated initializer has two entry-points. One performs allocation 
(i.e., the "allocating" entry) before continuing at the second entrypoint 
which does the initialization (i.e., the "initializing" entrypoint). 
Here's an example of `MyClass.init(final:)`, which is a designated initializer,
with its two entry-points:

```swift
// MyClass.__allocating_init(final:)
sil hidden [exact_self_class] [ossa] @$s4test7MyClassC5finalACSi_tcfC : $@convention(method) (<...>, @thick MyClass.Type) -> @owned MyClass {
bb0(%0 : $Int, %1 : $@thick MyClass.Type):
  %2 = alloc_ref $MyClass
  // function_ref MyClass.init(final:)
  %3 = function_ref @$s4test7MyClassC5finalACSi_tcfc : $@convention(method) (Int, @owned MyClass) -> @owned MyClass
  %4 = apply %3(%0, %2) : $@convention(method) (Int, @owned MyClass) -> @owned MyClass // user: %5
  return %4 : $MyClass
}

// MyClass.init(final:)
sil hidden [ossa] @$s4test7MyClassC5finalACSi_tcfc : $@convention(method) (Int, @owned MyClass) -> @owned MyClass {
bb0(<...>, %1 : @owned $MyClass):
  %4 = mark_uninitialized [rootself] %1 : $MyClass
  
  // ... initialize MyClass ...
  
  %11 = copy_value %4 : $MyClass
  destroy_value %4 : $MyClass
  return %11 : $MyClass
}
```

In the mangling of these entrypoint labels, the uppercase `C` suffix indicates
that it's the allocating entrypoint, whereas the lowercase `c` is the 
initializing entrypoint. Only the allocating entrypoint is published in the
type's vtable:

```swift
sil_vtable MyClass {
  // ...
  #MyClass.init!allocator: (MyClass.Type) -> (<...>) -> MyClass : @$s4test7MyClassC5finalACSi_tcfC	// MyClass.__allocating_init(final:)
}
```

The initializing entrypoint is only referenced by either it's corresponding
allocating entrypoint, or by a sub-class that is delegating up in a `super.init`
call. For example, if we had:

```swift
class MyClass {
  var x: Int
  init(final x: Int) {
    self.x = x
  }
}

class MyDerivedClass: MyClass {
  var y: Int
  init(subFinal y: Int) {
    self.y = y
    super.init(final: y)
  }
}
```

Then the `super.init(final: y)` call directly invokes `MyClass.init(final:)`'s
initializing entrypoint, bypassing its allocating init. Here's what that looks
like in SIL:

```
// MyDerivedClass.__allocating_init(final:)
sil hidden [exact_self_class] [ossa] @$s4test14MyDerivedClassC5finalACSi_tcfC : $@convention(method) (Int, @thick MyDerivedClass.Type) -> @owned MyDerivedClass {
  // ... calls $s4test14MyDerivedClassC5finalACSi_tcfc in the usual way ...
}

// MyDerivedClass.init(final:)
sil hidden [ossa] @$s4test14MyDerivedClassC5finalACSi_tcfc : $@convention(method) (Int, @owned MyDerivedClass) -> @owned MyDerivedClass {
bb0(%0 : $Int, %1 : @owned $MyDerivedClass):
  %2 = alloc_box ${ var MyDerivedClass }, let, name "self"
  %3 = mark_uninitialized [derivedself] %2 : ${ var MyDerivedClass }
  %4 = begin_borrow [lexical] %3 : ${ var MyDerivedClass }
  %5 = project_box %4 : ${ var MyDerivedClass }, 0
  debug_value %0 : $Int, let, name "y", argno 1
  store %1 to [init] %5 : $*MyDerivedClass
  
  // ... initialize self.y ...
  
  // perform the super call. notice the ownership transfer to the super.init.
  %14 = load [take] %5 : $*MyDerivedClass
  %15 = upcast %14 : $MyDerivedClass to $MyClass
  // function_ref MyClass.init(final:)
  %16 = function_ref @$s4test7MyClassC5finalACSi_tcfc : $@convention(method) (Int, @owned MyClass) -> @owned MyClass // user: %17
  %17 = apply %16(%0, %15) : $@convention(method) (Int, @owned MyClass) -> @owned MyClass // user: %18
  %18 = unchecked_ref_cast %17 : $MyClass to $MyDerivedClass
  store %18 to [init] %5 : $*MyDerivedClass       // id: %19
  
  // return as usual
  %20 = load [copy] %5 : $*MyDerivedClass
  end_borrow %4 : ${ var MyDerivedClass }
  destroy_value %3 : ${ var MyDerivedClass }
  return %20 : $MyDerivedClass
}
```

# Actors

There does not exist a sub-actor that inherits from some other actor in the type
system. As a result, the `convenience` keyword is not required for actor 
initializers in the source code. Without inheritance, only the allocating
entry-points can ever be used by an actor. 

Nevertheless, internally the compiler will still differentiate between 
convenience and designated initializers. So everything discussed
earlier for classes also apply to actors. The body of the initializer determines
whether the compiler internally treats it as `convenience` or not. For example,
an internally designated initializer for an actor still emits two entry-points,
but the initializing entrypoint is exclusively used by its corresponding
allocating entrypoint.


 
