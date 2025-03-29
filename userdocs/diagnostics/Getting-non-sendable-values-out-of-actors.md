# Getting non-sendable values out of actors

When an actor is used to protect a non-sendable value, the compiler will enforce that the non-sendable value remains in the actor's isolation domain. Accessing an actor's property of non-sendable type from a different concurrency region will be diagnosed when complete concurrency checking is enabled:

For example:

```swift
class BankAccount {
    var balance = 0
}

actor Bank {
    let name = "My Bank"
    let account = BankAccount()
}

nonisolated func depositAndPrintBalance(in bank: Bank, by value: Int) async {
    let bankName = await bank.name // Fine, `name` is Sendable.
    let account = await bank.account // ❌ Non-sendable type 'BankAccount' of property 'acount' cannot exit actor-isolated context
    account.balance += value
    print("Current balance in \(bankName): \(account.balance)")
}
```

Non-sendable types in actor-isolated properties can't be safely accessed concurrently, even when declared with `let`. This is because a `let` declaration can hold a type with internal mutable state, like `BankAccount`'s `balance` in the above example.

A useful pattern when working with with actors is to only use `Sendable` types to get information in and out of the actor. You may be able to achieve this by writing the functions directly in the actor:

```swift
class BankAccount {
    var balance = 0
}

actor Bank {
    let name = "My Bank"
    let account = BankAccount()

    func depositAndPrintBalance(_ deposit: Int) {
        account.balance += deposit // ✅
        print("Current balance in \(name): \(account.balance)")
    }
}
```

Or, if using a global actor like `@MainActor`, by annotating the call sites that need to access the non-sendable types so they never have to leave the concurrency region isolated to the global actor:

```swift
class BankAccount {
    var balance = 0
}

@MainActor 
final class Bank {
    let name = "My Bank"
    let account = BankAccount()
}

@MainActor 
func depositAndPrintBalance(in bank: Bank, by value: Int) {
    let account = bank.account // ✅
    account.balance += value
    print("Current balance in \(bank.name): \(bank.balance)")
}
```

By moving entire functions into an actor-isolated region, you may also be able to reduce the number of suspension points, or even make the function syncrhonous. This can result in simpler code, as the compiler guarantees that no other code can modify the actor protected state while you're running code synchronously on that actor.
