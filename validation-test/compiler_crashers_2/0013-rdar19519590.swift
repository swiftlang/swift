// RUN: not --crash %target-swift-frontend %s -emit-ir

protocol SourceTargetTransformable {
    typealias Source
    typealias Target
    typealias Transformer = Source -> Target
}


struct PiecewiseTransformedGeneratorOf<
                                        Source,
                                        Target,
                                        SourceGenerator: GeneratorType,
                                        TransformerGenerator: GeneratorType,
                                        Transformable: SourceTargetTransformable
                                      where
                                        Transformable.Source == Source,
                                        Transformable.Target == Target,
                                        SourceGenerator.Element == Source,
                                        TransformerGenerator.Element == Transformable.Transformer
                                      >
       : GeneratorType {
    typealias Element = Target
    
    var sourceGenerator: SourceGenerator
    var transformerGenerator: TransformerGenerator
    
    mutating func next() -> Element? {
        let source: Transformable.Source? = sourceGenerator.next()
        if let source: Transformable.Source = source {
            let transformer: Transformable.Transformer? = transformerGenerator.next()
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
    typealias Generator = PiecewiseTransformedGeneratorOf<Source, Target, SourceSequence.Generator, TransformerSequence.Generator, Transformable>
    
 
    let inputs: SourceSequence
    let transformers: TransformerSequence
    
    init(inputs: SourceSequence, transformers: TransformerSequence) {
        self.inputs = inputs
        self.transformers = transformers
    }
    
    func generate() -> Generator {
        return PiecewiseTransformedGeneratorOf(sourceGenerator: inputs.generate(), transformerGenerator: transformers.generate())
    }
}
