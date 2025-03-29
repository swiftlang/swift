# Non-sendable types and Objective-C interoperability

Actor-isolated methods that are `async` can be exposed to Objective-C using `@objc`. These methods are imported into Objective-C with a completion handler parameter. When Objective-C code calls any of these methods, a detached task that calls the `async` Swift method will be created, executed in the concurrency domain of the actor, and eventually forward the results to the completion handler.

Since Objective-C does not have knowledge of actor isolation, any types involved in the method's signature —either as parameters or as results— are crossing an actor boundary, and are required to be `Sendable`. Otherwise, an error will be diagnosed under strict concurrency checking. 

For example:

```swift
@objc 
class MyObjCModel: NSObject {
    var mutableState = 10
}

actor Repository: NSObject {
    
    var storedModel = MyObjCModel()
        
    @objc 
    func getModel() async -> MyObjCModel { // ❌ Non-sendable type 'MyObjCModel' returned by actor-isolated '@objc' instance method 'getModel()' cannot cross actor boundary
        return storedModel
    }
    
    @objc 
    func setModel(to model: MyObjCModel) async { // ❌ Non-sendable parameter type 'MyObjCModel' of actor-isolated '@objc' instance method 'setModel(to:)' cannot cross actor boundary
        storedModel = model
    }
}
```

Here, `getModel() async` provides a non-sendable `MyObjCModel` type back to Objective-C, but the Objective-C code receiving the `MyObjCModel` is not part of the actor's concurrency domain, which means that an actor boundary is crossed. Similarly, `setModel(to model: MyObjCModel) async` is passing a `MyObjCModel` from Objective-C code (which can't be isolated to the actor) into the actor's concurrency domain, which also means an actor boundary is crossed. This is not safe for non-sendable types.

There are a few possible ways to address this issue.

## Make all parameters and results `Sendable`

If all the parameters and result types of the method are `Sendable`, it's safe to send them across any actor boundaries.

```swift
@objc 
final class MyObjCModel: NSObject, Sendable {
    let state = 10
}

actor Repository: NSObject {
    
    var storedModel = MyObjCModel()
        
    @objc func getModel() async -> MyObjCModel { // ✅
        return storedModel
    }
    
    @objc func setModel(to model: MyObjCModel) async { // ✅
        storedModel = model
    }
}
```
Types can be made `Sendable` by an explicit `Sendable` conformance, a `@MainActor` annotation, or even with `@unchecked Sendable` when the type has been made thread safe by a different synchronization mechanism like a lock or a queue.

## Use a global actor like `@MainActor`

Another solution is to use a global actor (like `@MainActor`) instead of an actor's instance to protect mutable state that needs to be accessed from Objective-C:

```swift
@objc 
class MyObjCModel: NSObject {
    var mutableState = 10
}

@MainActor 
final class Repository: NSObject {
    
    var storedModel = MyObjCModel()
        
    @objc func getModel() async -> MyObjCModel { // ✅
        return storedModel
    }
    
    @objc func setModel(to model: MyObjCModel) async { // ✅
        storedModel = model
    }
}
```
However, on the Objective-C side, care must be taken to only call these methods from the global actor's executor. For the main actor, that means only calling these methods from the main thread.
