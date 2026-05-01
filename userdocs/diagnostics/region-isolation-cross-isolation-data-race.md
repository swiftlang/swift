# Cross-isolation data race (RegionIsolationCrossIsolationDataRace)

## Overview

The region-based isolation checker tracks values using regions, which are conservative approximations of object graphs. A region can be either non-isolated or isolated to a single concurrency domain (an actor, a global actor, or the current task), but it can never be isolated to more than one concurrency domain at a time.

This error occurs when an operation would merge two regions that are isolated to different concurrency domains. Merging such regions would allow values from both domains to be referenced through the same object graph, risking concurrent access and data races.

Common examples include assigning an actor-isolated value to a variable accessible from a different isolation domain, passing values from different isolation domains as arguments to the same function call, or casting a value to a type with different isolation.

To resolve this, ensure that values from different isolation domains are not mixed together in ways that could allow concurrent access. Consider making the types involved conform to `Sendable`, or restructuring the code so that values are only accessed from a single isolation domain.
