namespace CurryOn.Tracing.Jaeger

open Jaeger
open Jaeger.Propagation
open Jaeger.Reporters
open Jaeger.Samplers
open Jaeger.Senders.Thrift

open CurryOn.Tracing

open OpenTracing
open OpenTracing.Propagation

open Microsoft.Extensions.Logging

type JaegerTracingProvider (configuration: IJaegerTracingConfiguration, logger: ILoggerFactory) =
    let transport = UdpSender(configuration.AgentHost, configuration.AgentPort, 0)
    let reporter = RemoteReporter.Builder().WithSender(transport).WithLoggerFactory(logger).Build()

    let sampler = 
        match configuration.SamplingMode with
        | Off -> ConstSampler(false) :> ISampler
        | All -> ConstSampler(true) :> ISampler
        | Probabilistic frequency -> ProbabilisticSampler(frequency) :> ISampler
        | RateLimiting rate -> RateLimitingSampler(rate) :> ISampler
        | Remote (interval, server) -> RemoteControlledSampler.Builder(configuration.ServiceName)
                                            .WithInitialSampler(ConstSampler(true))
                                            .WithPollingInterval(interval)
                                            .WithSamplingManager(HttpSamplingManager(server))
                                            .Build() :> ISampler

    let codec = B3TextMapCodec()

    let injector =
        {new Injector<ITextMap>() with 
            member __.Inject (context: SpanContext, carrier: ITextMap) =
                codec.Inject(context, carrier) 
        }

    let extractor =
        {new Extractor<ITextMap>() with 
            member __.Extract (carrier: ITextMap) =
                codec.Extract(carrier) 
        }

    let tracer = Tracer.Builder(configuration.ServiceName)
                       .WithLoggerFactory(logger)
                       .WithSampler(sampler)
                       .WithReporter(reporter)
                       .RegisterInjector(BuiltinFormats.HttpHeaders, injector)
                       .RegisterExtractor(BuiltinFormats.HttpHeaders, extractor)
                       .Build()

    member internal __.Tracer = tracer

    interface IDistributedTracing with
        member __.Enabled = configuration.Enabled
        member __.Level = configuration.Level
        member __.Tracer = tracer :> ITracer
        

