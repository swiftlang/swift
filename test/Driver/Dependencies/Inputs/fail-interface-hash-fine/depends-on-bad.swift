# Fine-grained v0
---
allNodes:
  - key:
      kind:            sourceFileProvide
      aspect:          interface
      context:         ''
      name:            dependsonbad.swiftdeps
    fingerprint:     22222222222222222222222222222222
    sequenceNumber:  0
    defsIDependUpon: [ 2 ]
    isProvides:      true
  - key:
      kind:            sourceFileProvide
      aspect:          implementation
      context:         ''
      name:            dependsonbad.swiftdeps
    fingerprint:     22222222222222222222222222222222
    sequenceNumber:  1
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            bad
    sequenceNumber:  2
    defsIDependUpon: [ ]
    isProvides:      false
...

