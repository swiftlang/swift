# Upcoming language features

Upcoming language features enable new (but potentially source breaking) functionality that be
enabled by default in an upcoming language mode.


## Overview

Upcoming language features allow the incremental adoption of language features that would otherwise
only be available in a new language mode, without having to fully migrate to that mode. They can be
enabled on the command line with `-enable-upcoming-feature <feature>`.

Some upcoming features have an additional "migration" mode, where the compiler will emit warnings
with fix-its to help migrate to that mode. This can be enabled with `-enable-upcoming-feature
<feature>:migrate`.


## Topics

- <doc:existential-any>
- <doc:member-import-visibility>
- <doc:nonisolated-nonsending-by-default>
