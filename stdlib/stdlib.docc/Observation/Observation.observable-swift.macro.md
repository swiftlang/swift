# ``Observation/Observable()``

@Metadata {
    @DocumentationExtension(mergeBehavior: override)
}

Defines and implements conformance of the Observable protocol.

This macro adds observation support to a custom type and conforms the type
to the ``Observation/Observable`` protocol. For example, the
following code applies the `Observable` macro to the type `Car` making it
observable:

    @Observable 
    class Car {
       var name: String = ""
       var needsRepairs: Bool = false
   
       init(name: String, needsRepairs: Bool = false) {
           self.name = name
           self.needsRepairs = needsRepairs
       }
    }
