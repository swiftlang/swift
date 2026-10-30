# Managing Actor Reentrancy

Keep an actor's state consistent when other calls run while a method is suspended.

## Overview

An actor protects its state by allowing only one piece of code to run on it at a
time. However, that guarantee doesn't extend across a potential suspension point.
When a method on an actor reaches an `await`, the actor is free to run other
work—including another call to the same method—before the original call resumes.
This behavior is called *reentrancy*, and it's the default for every actor in Swift.

Reentrancy exists so actors stay responsive. If an actor couldn't be reentered while
one of its methods was suspended, a single slow operation would block every other
piece of code that needs to talk to that actor, and two actors that await each other
in this way could deadlock. Allowing reentrancy avoids that: The actor makes
progress on other work while a long-running asynchronous operation is in flight.

The tradeoff is that you can't assume an actor's state stays the same across an
`await`. Code that reads state, awaits, and then acts on what it read may find that
the state has changed in the meantime, because another call ran during the
suspension.

![A timeline with two calls to the same actor method. The first call runs, then suspends while its download is in flight. The second call reenters the actor during that suspension, runs, and starts its own download. Each call later resumes and finishes.](reentrancy-interleaving)

This article shows reentrancy working as intended, then three ways the same
mechanism can go wrong once two calls share state, and how to avoid each one. For
an introduction to actors, see
[Concurrency](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/concurrency)
in *The Swift Programming Language*.

### Keep the Actor Responsive

Reentrancy isn't only a risk to manage—it's also what makes an actor usable under
load. Consider an actor that downloads and caches images:

```swift
actor ImageLoader {
    private var cache: [String: Image] = [:]

    func image(for id: String) async throws -> Image {
        if let cached = cache[id] {
            return cached
        }

        print("Downloading image \(id).")
        let image = try await downloadImage(for: id)
        cache[id] = image
        print("Cached image \(id).")
        return image
    }
}
```

Consider two calls to `image(for:)` for two *different* IDs, made close
together—a photo grid loading several thumbnails at once, for example. The first
call checks `cache`, finds nothing for `"1234"`, and reaches the `await` while
downloading. Because the actor is reentrant, the second call doesn't wait for that
download to finish: It runs immediately, checks `cache` for the unrelated `"5678"`,
finds nothing there either, and starts its own download. Both downloads proceed
concurrently instead of one blocking behind the other.

Reading and writing `cache` here is always safe. An actor allows only one task to
access its mutable state at a time, which is exactly what makes it safe for code in
multiple tasks to interact with the same actor instance. Reentrancy only changes the
order calls can interleave *between* suspension points; it never lets two of them
access `cache` at the same instant.

If both calls reach the `await` before either finishes, one possible interleaving
prints:

```swift
let loader = ImageLoader()
async let first = loader.image(for: "1234")
async let second = loader.image(for: "5678")
_ = try await (first, second)
// Prints "Downloading image 1234."
// Prints "Downloading image 5678."
// Prints "Cached image 5678."
// Prints "Cached image 1234."
```

The order doesn't matter: `cache["1234"]` and `cache["5678"]` are two independent
entries, so there's no shared state for the interleaving to disturb. Without
reentrancy, the second call would wait for the first one's entire download to
finish, even though the two requests share nothing.

That's reentrancy working as intended. But the same mechanism can go wrong in more
than one way once two calls *do* share state. The following sections describe three
of those problems and how to avoid each one.

### Avoid Duplicate Work

Using the same `image(for:)` method, consider two calls with the same `id`, made
close together. The first call checks `cache`, finds nothing, and reaches the
`await` while downloading the image. Because the actor is reentrant, the second call
can run before the first one resumes. It also checks `cache` and also finds
nothing—the first call hasn't written the result yet—so it starts a second,
redundant download. Both calls eventually write to `cache[id]`, so the image is
downloaded twice. This is wasteful, but not wrong: Whichever download finishes last
leaves a correct image in the cache.

If both calls reach the `await` before either finishes, one possible interleaving
prints:

```swift
let loader = ImageLoader()
async let first = loader.image(for: "1234")
async let second = loader.image(for: "1234")
_ = try await (first, second)
// Prints "Downloading image 1234."
// Prints "Downloading image 1234."
// Prints "Cached image 1234."
// Prints "Cached image 1234."
```

![A timeline with two calls to image(for:) for the same ID. Both calls miss the cache and start their own download of the same image, overlapping in time, and each writes the image to the cache.](reentrancy-duplicate-work)

Wasteful doesn't mean harmless. Under enough concurrent load—a cold cache after a
deployment, for example—redundant downloads for the same `id` can compound faster
than any of them finish, a pattern sometimes called a *cache stampede* or
*thundering herd*. No single
request does anything wrong, but together they can overload a system.

Swift doesn't let you make an actor non-reentrant, and doing so would risk the
deadlocks described above. Instead, record that a download is in progress *before*
awaiting anything, so a reentrant call finds that record and shares the result
instead of starting its own download:

```swift
actor ImageLoader {
    private enum CacheEntry {
        case inProgress(Task<Image, Error>)
        case ready(Image)
    }

    private var cache: [String: CacheEntry] = [:]

    func image(for id: String) async throws -> Image {
        if let entry = cache[id] {
            switch entry {
            case .ready(let image):
                return image
            case .inProgress(let task):
                return try await task.value
            }
        }

        let task = Task {
            print("Downloading image \(id).")
            return try await downloadImage(for: id)
        }
        cache[id] = .inProgress(task)

        do {
            let image = try await task.value
            cache[id] = .ready(image)
            return image
        } catch {
            cache[id] = nil
            throw error
        }
    }
}
```

The key change is that `cache[id] = .inProgress(task)` runs synchronously, before
the first `await`. The first call then awaits `task.value`, and when the download
finishes, it updates `cache[id]` to `.ready`, or clears the entry if the download
throws. By the time a second call can reenter the actor, the in-progress download is
already recorded. That call finds the `.inProgress` entry and awaits the same task
instead of starting a new one, so it receives the same result—or the same
error—as the first call.

![A timeline with two calls to image(for:) for the same ID. The first call records the download as a task before awaiting it. The second call reenters, finds that task, and awaits it too. Only one download happens.](reentrancy-shared-task)

Even if both calls reach the actor before either finishes, only the first one starts
a download:

```swift
let loader = ImageLoader()
async let first = loader.image(for: "1234")
async let second = loader.image(for: "1234")
_ = try await (first, second)
// Prints "Downloading image 1234."
```

### Avoid a Stale Overwrite

Consider an actor that caches the latest price for a stock symbol:

```swift
actor PriceCache {
    private var cache: [String: Double] = [:]

    func price(for symbol: String) async throws -> Double {
        if let cached = cache[symbol] {
            return cached
        }

        let price = try await fetchPrice(for: symbol)
        print("Caching \(symbol) at \(price).")
        cache[symbol] = price
        return price
    }
}
```

Consider two calls to `price(for:)` for the same `symbol`, made close together while
the price is changing. The first call checks `cache`, finds nothing, and reaches the
`await` while fetching. Because the actor is reentrant, the second call can run
before the first one resumes: It also checks `cache`, also finds nothing, and starts
its own fetch. Unlike the redundant downloads in the previous example, these two
fetches can return different values, because the price changed between them. And
nothing ties the order in which the calls write to `cache[symbol]` to the order in
which the requests went out.

If the response to the *earlier* request arrives—and is written—*after* the
response to the later one, its price overwrites the newer, correct value.
`cache[symbol]` ends up holding a stale price that looks just like a current one, and
every later call to `price(for:)` returns it.

If the two fetches finish in the opposite order from the requests that started them,
one possible interleaving prints:

```swift
let prices = PriceCache()
async let first = prices.price(for: "ACME")
async let second = prices.price(for: "ACME")
_ = try await (first, second)
// Prints "Caching ACME at 105.0."
// Prints "Caching ACME at 100.0."
```

`cache["ACME"]` now holds `100.0`, the stale price, even though `105.0` is more
recent. There's no error and no signal that anything went wrong: Every later caller
receives the wrong number.

![A timeline with two calls to price(for:) for the same symbol. The second call's fetch finishes first and writes the new price, 105, to the cache. The first call's slower fetch finishes afterward and overwrites the cache with the stale price, 100.](reentrancy-stale-overwrite)

#### Serialize the Method with an Asynchronous Lock

One way to avoid the stale overwrite is to hold a lock around the entire method, so
only one call to `price(for:)`—for any symbol—runs at a time. The lock needs to
stay held across the `await` on `fetchPrice(for:)`, so use a small actor as the lock.
An actor is already exclusive and already supports `await`: Its own state—whether
it's `locked`, and which callers are waiting—is protected the same way as any other
actor's state. A call that finds the lock held doesn't block a thread; it suspends on
a continuation and gives up its thread, like any other `await`:

```swift
actor AsyncMutex {
    private var locked = false
    private var waiters: [CheckedContinuation<Void, Never>] = []

    func acquire() async {
        if !locked {
            locked = true
            return
        }

        await withCheckedContinuation { (continuation: CheckedContinuation<Void, Never>) in
            waiters.append(continuation)
        }

        locked = true
    }

    nonisolated func release() {
        Task { await _release() }
    }

    private func _release() {
        if !waiters.isEmpty {
            waiters.removeFirst().resume()
            return
        }

        locked = false
    }
}
```

The `acquire()` method returns immediately if nothing else holds the lock.
Otherwise, it suspends the caller on a ``Swift/CheckedContinuation`` and records that
continuation in `waiters`, in order. The `release()` method is `nonisolated` so a
`defer` statement can call it without an `await`. It hands the lock to the next
waiter if there is one, or marks the lock as free.

In the following example, three tasks call `criticalWork(_:)`, which holds the lock
across a suspension point. If `A` reaches `acquire()` first, followed by `B` and then
`C`, the example prints:

```swift
let lock = AsyncMutex()

func criticalWork(_ label: String) async {
    await lock.acquire()
    defer { lock.release() }

    print("\(label) acquired the lock.")
    try? await Task.sleep(for: .milliseconds(100))
    print("\(label) finished.")
}

async let a: Void = criticalWork("A")
async let b: Void = criticalWork("B")
async let c: Void = criticalWork("C")
_ = await (a, b, c)
// Prints "A acquired the lock."
// Prints "A finished."
// Prints "B acquired the lock."
// Prints "B finished."
// Prints "C acquired the lock."
// Prints "C finished."
```

Because `acquire()` is itself asynchronous, the `AsyncMutex` actor is reentrant too:
A later call to `acquire()` can start before an earlier one returns. It might look
like two calls could both pass the `if !locked` check and both acquire the lock.
That never happens, because the check and the assignment `locked = true` run
synchronously, with no `await` between them. Actor isolation guarantees that only
one call's synchronous code runs at a time, so the first caller to reach that check
sets `locked` to `true` before any other call can read it.

`B` and `C` both suspend while `A` holds the lock, and neither one acquires it until
the current holder wakes the next waiter. A suspended call to `acquire()` stops
running entirely at `await withCheckedContinuation`—it doesn't poll or retry—and
stays suspended until `release()` hands it the lock.

![A timeline with three tasks using the same AsyncMutex. A holds the lock first. B and C wait, in that order. When A releases the lock, B gets it. When B releases the lock, C gets it.](reentrancy-fair-lock)

Because `waiters` is an ordinary array that `acquire()` appends to and `release()`
removes from the front of, waiters are served in the order they arrived. `A`'s
release wakes `B`, not `C`, because `B` started waiting first. That order is always
well defined: Each call's check of `locked` and the `append(_:)` call that follows
run without interruption under actor isolation, so there's never a tie between two
arrivals. As a result, `AsyncMutex` is a *fair* lock: A later arrival can't cut in
line and starve an earlier one.

> Important: `AsyncMutex` has the same risk as any manual lock. Nothing ties a call
> to `acquire()` to a call to `release()`, so the type system doesn't stop you from
> acquiring the lock and never releasing it. If you skip the release on an early
> return or a thrown error, the lock stays held, and every later call to `acquire()`
> suspends forever. Always release the lock when the current scope exits; a `defer`
> statement immediately after `await lock.acquire()` is the simplest way to do that.

`PriceCache` uses one `AsyncMutex` to guard its entire method body:

```swift
actor PriceCache {
    private var cache: [String: Double] = [:]
    private let lock = AsyncMutex()

    func price(for symbol: String) async throws -> Double {
        await lock.acquire()
        defer { lock.release() }

        if let cached = cache[symbol] {
            return cached
        }

        let price = try await fetchPrice(for: symbol)
        print("Caching \(symbol) at \(price).")
        cache[symbol] = price
        return price
    }
}
```

A second call for the same symbol now waits in `acquire()` instead of racing the
first. By the time it resumes, `cache[symbol]` already holds the result, so it
returns immediately without fetching again:

```swift
let prices = PriceCache()
async let first = prices.price(for: "ACME")
async let second = prices.price(for: "ACME")
_ = try await (first, second)
// Prints "Caching ACME at 105.0."
```

The tradeoff is that the lock doesn't distinguish between symbols. A call for
`"MSFT"` made while `"ACME"` is still fetching also waits, even though the two share
nothing, instead of proceeding concurrently as reentrancy would otherwise allow:

```swift
let prices = PriceCache()
async let first = prices.price(for: "ACME")
async let second = prices.price(for: "MSFT")
_ = try await (first, second)
// Prints "Caching ACME at 105.0."
// Prints "Caching MSFT at 310.0."
```

![A timeline with two calls to price(for:) on a PriceCache that holds one lock around the whole method. The call for ACME holds the lock while it fetches. The call for MSFT, an unrelated symbol, waits for the lock and fetches only after the ACME call finishes.](reentrancy-one-lock)

`AsyncMutex` avoids the stale overwrite, but it also gives up the concurrency that
reentrancy provides: Calls for unrelated symbols run one at a time, instead of
proceeding concurrently like the two downloads in the first `ImageLoader` example.
Each call also waits for every call ahead of it in the queue to finish, so the more
calls that are waiting, the longer each one takes. For example, if ten calls for
different symbols arrive together and each fetch takes 200 milliseconds, the last
call returns after about two seconds instead of about 200 milliseconds. For a
simple, reusable lock, that's often an acceptable cost, but consider it for an actor
that handles many independent keys.

#### Choose Between a Recorded Task and a Lock

A lock isn't the only way to avoid this stale overwrite. The technique `ImageLoader`
uses to avoid duplicate work—recording an in-progress `Task` before the first
`await`—also prevents it. A reentrant call for the same `symbol` finds the recorded
task and awaits its result instead of starting a second fetch. With at most one fetch
in flight per symbol, there's only one price to write, so there's no slower response
left to overwrite a newer one. That approach needs no lock, and it lets calls for
unrelated symbols fetch concurrently.

The difference between the two approaches is what they coordinate. A recorded task
lets calls share the result of work that's already in progress: A call that asks for
the same result waits for it instead of starting the work again. A recorded task
doesn't stop calls that do different work from running at the same time. A lock
excludes every other call from the critical section until the current call finishes,
regardless of what each call does. Use a recorded task when calls need the same
result, and use a lock when operations must not overlap—for example, when a price
service accepts only one request at a time.

> Note: The standard library's
> [`Mutex`](https://developer.apple.com/documentation/synchronization/mutex)
> type doesn't work here. The race isn't about a single read or write of
> `cache[symbol]`—actor isolation already makes each of those safe. It's about the
> gap between checking the cache and writing to it, and closing that gap means
> holding the lock across the `await` on `fetchPrice(for:)`. The closure you pass to
> `Mutex`'s `withLock(_:)` method is synchronous, so the compiler rejects an `await`
> inside it. Even without that restriction, holding a lock that blocks a thread
> across a suspension point is dangerous in Swift's cooperative thread pool: Tasks
> waiting for the lock can block every thread in the pool, leaving no thread for the
> suspended task that holds the lock to resume on and release it.

### Avoid a Double Credit

Reentrancy doesn't need a cache to cause problems. Consider a `Game` actor that
records baskets in a basketball game played to a winning score, and credits the
winning basket to the player who scored it:

```swift
actor Game {
    let winningScore: Int
    private(set) var score = 0

    init(winningScore: Int) {
        self.winningScore = winningScore
    }

    var isOver: Bool { score >= winningScore }

    // Returns true when this basket won the game.
    func recordBasket(_ points: Int, by player: String) async -> Bool {
        guard !isOver else { return false }

        await updateScoreboard(points, by: player)

        score += points
        let wonTheGame = isOver
        if wonTheGame {
            print("\(player) scored the winning basket.")
        }
        return wonTheGame
    }
}
```

Consider two baskets recorded close together near the end of the game, each one
enough to win it. The first call checks `isOver`, finds that the game is still on,
and reaches the `await`. Because the actor is reentrant, the second call runs before
the first one resumes. It also checks `isOver` against the same, unchanged
`score`—the first call hasn't added its points yet—and also passes the guard. Both
calls later add their points, both find that the game is over, and both report the
winning basket.

If both calls reach the `await` before either resumes, one possible interleaving
prints:

```swift
let game = Game(winningScore: 2)
async let ana = game.recordBasket(2, by: "Ana")
async let kai = game.recordBasket(2, by: "Kai")
_ = await (ana, kai)
// Prints "Ana scored the winning basket."
// Prints "Kai scored the winning basket."
```

Two different players are both credited with the same winning basket.

![A timeline with two calls to recordBasket(_:by:) on the same Game. Ana's call checks that the game isn't over and suspends. Kai's call reenters, also finds the game not over, and suspends. Both calls resume, add their points, and both report the winning basket.](reentrancy-double-credit)

Unlike the caching examples, `Game` has no in-progress work to record. Instead,
avoid the double credit by committing the entire outcome synchronously, before the
method's only `await`:

```swift
actor Game {
    let winningScore: Int
    private(set) var score = 0

    init(winningScore: Int) {
        self.winningScore = winningScore
    }

    var isOver: Bool { score >= winningScore }

    func recordBasket(_ points: Int, by player: String) async -> Bool {
        guard !isOver else { return false }

        // Commit the outcome synchronously. There's no await between
        // reading the score and updating it, so nothing can reenter.
        score += points
        let wonTheGame = isOver
        if wonTheGame {
            print("\(player) scored the winning basket.")
        }

        // Suspend only after the outcome is decided.
        await updateScoreboard(points, by: player)

        return wonTheGame
    }
}
```

Because `score += points` and the check for the win both run before the method's
only `await`, no other call can run between them. By the time a second call reaches
`guard !isOver`, `score` already includes the first basket, so at most one call
reports the winning basket.

Even if two winning baskets reach the actor close together, the example prints:

```swift
let game = Game(winningScore: 2)
async let ana = game.recordBasket(2, by: "Ana")
async let kai = game.recordBasket(2, by: "Kai")
_ = await (ana, kai)
// Prints "Ana scored the winning basket."
```

![A timeline with two calls to recordBasket(_:by:) on the same Game. Ana's call adds her points and records the win before it suspends. Kai's call reenters, finds that the game is already over, and returns without credit.](reentrancy-double-credit-fix)

### Follow the General Rule

Don't assume that state you read before an `await` is still valid after it. If a
method needs to act on state across a suspension point, either check that state again
after the `await`, or—as in the examples above—commit a consistent change to the
state synchronously before suspending, so any call that reenters the actor sees a
coherent picture.

Reentrancy applies only at potential suspension points. Code between two `await`
expressions, or code that never awaits, still runs without interruption.

For a video introduction to actors and reentrancy, see
[Protect mutable state with Swift actors](https://developer.apple.com/videos/play/wwdc2021/10133/)
from WWDC21.
