namespace CurryOn.Serialization

open CurryOn
open CurryOn.DependencyInjection
open System

module Serializer =
    let private useSerializer (f: ISerializer -> Result<_,_>) =
        injected {
            let! serializer = inject<ISerializer>() |> Injected.mapError SerializerNotFound
            return! f serializer
        }

    let private useSerializerAsync (f: ISerializer -> AsyncResult<_,_>) =
        injectedAsync {
            let! serializer = injectAsync<ISerializer>() |> InjectedAsync.mapError SerializerNotFound
            return! f serializer
        }

    let toBytes (object: 't) =
        useSerializer <| fun serializer -> serializer.Serialize(object)

    let toString (object: 't) =
        useSerializer <| fun serializer -> serializer.SerializeToString(object)

    let toStream stream (object: 't) =
        useSerializer <| fun serializer -> serializer.SerializeToStream stream object

    let toContent (object: 't) =
        useSerializer <| fun serializer -> serializer.SerializeToContent(object)

    let parseBytes<'t> bytes =
        useSerializer <| fun serializer -> serializer.Deserialize<'t>(bytes)

    let parseString<'t> str =
        useSerializer <| fun serializer -> serializer.DeserializeString<'t>(str)

    let parseStream<'t> stream =
        useSerializer <| fun serializer -> serializer.DeserializeStream<'t>(stream)

    let parseContent<'t> content =
        useSerializerAsync <| fun serializer -> serializer.DeserializeContent<'t>(content)

    let parseBytesAs (objectType: Type) bytes =
        useSerializer <| fun serializer -> serializer.DeserializeAsType objectType bytes

    let parseStringAs (objectType: Type) str =
        useSerializer <| fun serializer -> serializer.DeserializeStringAsType objectType str

    let parseStreamAs (objectType: Type) stream =
        useSerializer <| fun serializer -> serializer.DeserializeStreamAsType objectType stream

    let parseContentAs (objectType: Type) content =
        useSerializerAsync <| fun serializer -> serializer.DeserializeContentAsType objectType content