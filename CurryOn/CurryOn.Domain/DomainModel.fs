namespace CurryOn.Domain

open CurryOn.Validation
open System

type IEvent =
    abstract member EventId: Guid

type Identity =
| UniqueId of Guid
| IntegerId of int64
| StringId of string

type IEntity =
    abstract member Id: Identity

type DomainModelAttribute = CLIMutableAttribute
type ValueObjectAttribute (dto: Type) = inherit ValidatedAttribute(dto)
type EntityAttribute (dto: Type) = inherit ValidatedAttribute(dto)
type AggregateAttribute (dto: Type) = inherit ValidatedAttribute(dto)