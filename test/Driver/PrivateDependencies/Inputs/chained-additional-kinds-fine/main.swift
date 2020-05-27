# Fine-grained v0
---
allNodes:
  - key:
      kind:            sourceFileProvide
      aspect:          interface
      context:         ''
      name:            main.swiftdeps
    fingerprint:     8276a546203ebde599da50b466729230
    sequenceNumber:  0
    defsIDependUpon: [ 4, 2 ]
    isProvides:      true
  - key:
      kind:            sourceFileProvide
      aspect:          implementation
      context:         ''
      name:            main.swiftdeps
    fingerprint:     8276a546203ebde599da50b466729230
    sequenceNumber:  1
    defsIDependUpon: [ ]
    isProvides:      true
  - key:
      kind:            dynamicLookup
      aspect:          interface
      context:         ''
      name:            z
    sequenceNumber:  2
    defsIDependUpon: [ 0 ]
    isProvides:      true
  - key:
      kind:            dynamicLookup
      aspect:          implementation
      context:         ''
      name:            z
    sequenceNumber:  3
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            a
    sequenceNumber:  4
    defsIDependUpon: [  ]
    isProvides:      false
...
