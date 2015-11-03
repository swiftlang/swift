// RUN: %target-swift-frontend %s -emit-ir

protocol SourceTargetTransformable {
    typealias Source
    typealias Target
    typealias Transformer = Source -> Target
}


struct PiecewiseTransformedIteratorOf<
                                        Source,
                                        Target,
                                        SourceIterator: IteratorProtocol,
                                        TransformerIterator: IteratorProtocol,
                                        Transformable: SourceTargetTransformable
                                      where
                                        Transformable.Source == Source,
                                        Transformable.Target == Target,
                                        SourceIterator.Element == Source,
                                        TransformerIterator.Element == Transformable.Transformer
                                      >
       : IteratorProtocol {
    typealias Element = Target
    
    var sourceIterator: SourceIterator
    var transformerIterator: TransformerIterator
    
    mutating func next() -> Element? {
        let source: Transformable.Source? = sourceIterator.next()
        if let source: Transformable.Source = source {
            let transformer: Transformable.Transformer? = transformerIterator.next()
            if let transformer: Transformable.Transformer = transformer {
                let tfunc: (Source -> Target)? = transformer as? (Source -> Target)
                if let tfunc = tfunc {
                    return tfunc(source)
                }
            }
        }
        return nil
    }
}

struct PiecewiseTransformedSequenceOf<
        SourceSequence: SequenceType,
        TransformerSequence: SequenceType,
        Transformable: SourceTargetTransformable
    where
        SourceSequence.Generator.Element == Transformable.Source,
        TransformerSequence.Generator.Element == Transformable.Transformer
        >: SequenceType {
    
    typealias Source = SourceSequence.Generator.Element
    typealias Target = Transformable.Target
    typealias Generator = PiecewiseTransformedIteratorOf<Source, Target, SourceSequence.Generator, TransformerSequence.Generator, Transformable>
    
 
    let inputs: SourceSequence
    let transformers: TransformerSequence
    
    init(inputs: SourceSequence, transformers: TransformerSequence) {
        self.inputs = inputs
        self.transformers = transformers
    }
    
    func generate() -> Generator {
        return PiecewiseTransformedIteratorOf(sourceIterator: inputs.generate(), transformerIterator: transformers.generate())
    }
}
