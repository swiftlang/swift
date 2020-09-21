# Fine-grained v0
---
allNodes:
  - key:
      kind:            sourceFileProvide
      aspect:          interface
      context:         ''
      name:            other.swiftdeps
    fingerprint:     e12419713170057a991bc883225f56fc
    sequenceNumber:  0
    defsIDependUpon: [ 4, 2]
    isProvides:      true
  - key:
      kind:            sourceFileProvide
      aspect:          implementation
      context:         ''
      name:            other.swiftdeps
    fingerprint:     e12419713170057a991bc883225f56fc
    sequenceNumber:  1
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            a
    sequenceNumber:  2
    defsIDependUpon: [ 0 ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          implementation
      context:         ''
      name:            a
    sequenceNumber:  3
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            b
    sequenceNumber:  4
    defsIDependUpon: [  ]
    isProvides:      false
...
