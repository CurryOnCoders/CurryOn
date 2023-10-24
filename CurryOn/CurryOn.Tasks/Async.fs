namespace CurryOn.Tasks

open System
open System.Threading
open System.Threading.Tasks

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Async =
    let ParallelThrottled<'a> (maxDegreeOfParallelism: int) (computations: Async<'a> seq) =
        Async.Parallel(computations, maxDegreeOfParallelism)