// RUN: %target-swift-frontend %s -emit-ir

protocol SourceTargetTransformable {
    associatedtype Source
    associatedtype Target
  
    // FIXME: should really be a typealias once we support that
    associatedtype Transformer = (Source) -> Target
}


struct PiecewiseTransformedIteratorOf<
                                        Source,
                                        Target,
                                        SourceIterator: IteratorProtocol,
                                        TransformerIterator: IteratorProtocol,
                                        Transformable: SourceTargetTransformable
                                      >
       : IteratorProtocol
      where
	Transformable.Source == Source,
	Transformable.Target == Target,
	SourceIterator.Element == Source,
	TransformerIterator.Element == Transformable.Transformer {
    typealias Element = Target
    
    var sourceIterator: SourceIterator
    var transformerIterator: TransformerIterator
    
    mutating func next() -> Element? {
        let source: Transformable.Source? = sourceIterator.next()
        if let source: Transformable.Source = source {
            let transformer: Transformable.Transformer? = transformerIterator.next()
            if let transformer: Transformable.Transformer = transformer {
                let tfunc: ((Source) -> Target)? = transformer as? ((Source) -> Target)
                if let tfunc = tfunc {
                    return tfunc(source)
                }
            }
        }
        return nil
    }
}

struct PiecewiseTransformedSequenceOf<
        SourceSequence: Sequence,
        TransformerSequence: Sequence,
        Transformable: SourceTargetTransformable
        >: Sequence
    where
        SourceSequence.Iterator.Element == Transformable.Source,
        TransformerSequence.Iterator.Element == Transformable.Transformer {
    
    typealias Source = SourceSequence.Iterator.Element
    typealias Target = Transformable.Target
    typealias Iterator = PiecewiseTransformedIteratorOf<Source, Target, SourceSequence.Iterator, TransformerSequence.Iterator, Transformable>
    
 
    let inputs: SourceSequence
    let transformers: TransformerSequence
    
    init(inputs: SourceSequence, transformers: TransformerSequence) {
        self.inputs = inputs
        self.transformers = transformers
    }
    
    func makeIterator() -> Iterator {
        return PiecewiseTransformedIteratorOf(sourceIterator: inputs.makeIterator(), transformerIterator: transformers.makeIterator())
    }
}
