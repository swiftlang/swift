# Synchronization: Linux Mutex

Implementation notes for `Synchronization.Mutex` on Linux
(`stdlib/public/Synchronization/Mutex/LinuxImpl.swift`).

The lock is a plain-futex mutex with a 3-state lock word, a bounded user-space
spin phase, and a kernel fallback via `FUTEX_WAIT` / `FUTEX_WAKE`.

## Lock word states

- `.unlocked` - free.
- `.locked` - held, no waiters parked in the kernel.
- `.contended` - held, at least one waiter parked in the kernel. The unlock
  path issues `FUTEX_WAKE` only when it observes this state, avoiding a syscall
  on the uncontended path.

## Tunables

- `spinTries` (20): spin-phase iteration budget per `_lockSlow` entry.
- `pauseBase` (64): CPU `pause` count per spin iteration, before jitter. Must
  be a power of two - the jitter is derived as `cycleCounter & (pauseBase - 1)`
  so that N concurrent spinners do not retry in lockstep.
- `maxActiveSpinners` (4): once this many threads are already in the kernel
  phase, new arrivals skip the spin loop and park immediately. Keeps the set
  of actively-spinning threads bounded so the lock holder's critical section
  runs without cache-line interference from spinners.

## Lock / unlock flow

```mermaid
flowchart TD
    subgraph lockflow ["_lock()"]
        fastCAS{{"compareExchange(.unlocked, .locked)"}}
        fastCAS -- "exchanged == true" --> locked([LOCKED])
        fastCAS -- "exchanged == false" --> slow([_lockSlow])

        slow --> stateCheck{"initialState == .contended?"}
        stateCheck -- no --> spinTry
        stateCheck -- yes --> depthCheck{"depth >= maxActiveSpinners?"}
        depthCheck -- no --> spinTry
        depthCheck -- yes --> parkClaim

        subgraph spinloop ["spin loop (repeat while spinsRemaining > 0)"]
            spinTry{{"compareExchange(.unlocked, .locked)"}} -- "exchanged == false, state == .locked" --> pause["pause CPU<br/>(pauseBase + per-thread jitter)"]
            pause --> spinsCheck{"spinsRemaining -= 1<br/>spinsRemaining > 0?"}
            spinsCheck -- yes --> spinTry
        end
        spinTry -- "exchanged == true" --> locked
        spinTry -- "state == .contended" --> parkClaim
        spinsCheck -- no --> parkClaim

        subgraph parkloop ["park loop (while true)"]
            parkClaim["storage.exchange(.contended)"] --> exchResult{"returned == .unlocked?"}
            exchResult -- no --> inc["slowPathDepth += 1<br/>(first miss only)"]
            inc --> wait[["_futexWait(expected: .contended)"]]
            wait -- "woken / EAGAIN / EINTR" --> parkClaim
        end
        exchResult -- yes --> dec["slowPathDepth -= 1<br/>(if previously bumped)"]
        dec --> locked
    end

    subgraph unlockflow ["_unlock()"]
        unlockSwap{{"storage.exchange(.unlocked)"}}
        unlockSwap -- "== .locked" --> done([done])
        unlockSwap -- "== .contended" --> wake[["_futexWake(count: 1)"]]
    end

    wake -. wakes parker .-> wait

    %% highlight loop back-edges (spin continue, park retry)
    linkStyle 9 stroke:#ff8800,stroke-width:2px
    linkStyle 16 stroke:#ff8800,stroke-width:2px
```
